{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Render where

import Linear
import Graphics.GL.Core46
import qualified Graphics.Rendering.FreeType.Internal as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FT
import qualified Graphics.Rendering.FreeType.Internal.Face as FT
import qualified Graphics.Rendering.FreeType.Internal.Vector as FT
import Foreign.C.String
import Control.Monad
import qualified Codec.Wavefront as Wavefront
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Codec.Picture as Juicy
import Data.Void
import Foreign hiding (rotate)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe

import Purity.Data

data ModelData = ModelData
  {modelVerticies :: [V3 GLfloat]
  ,modelTexCoords :: Maybe [V2 GLfloat]
  ,modelNormals :: Maybe [V3 GLfloat]
  ,modelIndicies :: [V3 GLushort]
  }

data DrawModel = DrawModel
  {indexCount :: GLsizei
  ,vaoName :: GLuint
  ,primitiveType :: GLenum
  ,elementType :: GLenum
  ,textureName :: Maybe GLuint
  }

data DrawInstructions = DrawIndexedModel DrawModel | DrawDirectly (IO ())

-- | The per-model information needed to render
data RenderSpec = RenderSpec
  {modelMatrix :: M44 GLfloat
  ,modelToRender :: DrawInstructions
  ,renderItemName :: String
  }

-- | The per-frame information needed to render
data RenderContext = RenderContext
  {viewMatrix :: M44 GLfloat
  ,projMatrix :: M44 GLfloat
  ,lightPos :: V3 GLfloat
  }

-- | All the context that must be passed from OpenGL initialization to the renderer
-- This information is per-program-run
data OpenGLInfo = OpenGLInfo
  {shaderProgram :: GLuint
  ,modelMatrixId :: GLint
  ,viewMatrixId :: GLint
  ,projectionMatrixId :: GLint
  ,lightLocationId :: GLint
  ,textureSamplerId :: GLint
  }

data ShaderProgram = ShaderProgram
  {shaderName :: GLuint
  ,shaderUniformNames :: [GLint]
  }

-- | Pretty(ish) print a matrix-like structure
prettyMatrix :: (Foldable f, Foldable g, Show a) => f (g a) -> String
prettyMatrix = (++"\n") . foldMap ((++"\n") . foldMap ((++" \t") . show))

drawModel :: GLint -> RenderSpec -> IO ()
drawModel modelMatrixId RenderSpec{..} = do
  sendMatrix "Model" modelMatrixId modelMatrix
  case modelToRender of
    DrawIndexedModel dm -> do
      logTick $ "Binding VAO for " ++ renderItemName ++ "..."
      glBindVertexArray (vaoName dm)
      case textureName dm of
        Just name -> do
          logTick $ "Using texture number " ++ show name ++ "..."
          glBindTexture GL_TEXTURE_2D name
        Nothing -> pure ()
      logTick $ "Drawing triangles for " ++ renderItemName ++ "..."
      glDrawElements (primitiveType dm) (indexCount dm) (elementType dm) nullPtr
    DrawDirectly i -> i

-- | A prepared vertex attribute
data VertexAttribute = VertexAttribute
  {attributeName :: String
  ,attributeObjectName :: GLuint
  ,attributeIndex :: GLuint
  ,attributeSize :: GLint
  ,attributeType :: GLenum
  ,attributeNormalized :: GLboolean
  ,attributeStride :: GLsizei
  ,attributeOffset :: Ptr Void
  }

-- | Load a png texture from a file
loadTexture :: FilePath -> IO GLuint
loadTexture texturePath = do
  logInfo $ "Generating texture name for " ++ texturePath ++ "..."
  textureId <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
  logInfo "Binding texture..."
  glBindTexture GL_TEXTURE_2D textureId

  logInfo $ "Loading texture juicily from file " ++ texturePath ++ "..."
  Right dynImage <- Juicy.readPng texturePath
  let testTexture = Juicy.convertRGB8 dynImage

  logInfo "Sending texture to GL..."
  Storable.unsafeWith (Juicy.imageData testTexture) $ glTexImage2D
    GL_TEXTURE_2D
    0 -- Mipmap level (0 full res)
    (fromIntegral GL_RGB) -- internal format
    (fromIntegral $ Juicy.imageWidth testTexture)
    (fromIntegral $ Juicy.imageHeight testTexture)
    0 -- This is 0 because OpenGL. L
    GL_RGB -- stored format
    GL_UNSIGNED_BYTE -- size of color component

  logInfo "Configuring mipmaps..."
  logInfo "  Magnify using linear filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  logInfo "  Minify using linear blending and filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  logInfo "Generating mipmaps..."
  glGenerateMipmap GL_TEXTURE_2D
  pure textureId

initFlatModel :: FilePath -> IO (DrawModel, ModelData)
initFlatModel modelPath = do
  logInfo $ "Initializing flat model " ++ modelPath ++ "..."
  (vao,[vbo,nbo,ibo]) <- generateVertexNames ["Vertex","Normal","Index"]
  logInfo "Binding VAO..."
  glBindVertexArray vao

  logInfo $ "Loading model " ++ modelPath ++ "..."
  model <- Wavefront.fromFile modelPath >>= \case
    Right m -> pure m
    Left e -> error e

  logInfo $ "Buffering vertex attributes for " ++ modelPath ++ "..."
  Just pos <- loadVertexAttr @(V3 GLfloat) model Wavefront.objLocations (\(Wavefront.Location x y z _w) -> V3 x y z) (VertexAttribute "position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr)
  mNor <- loadVertexAttr @(V3 GLfloat) model Wavefront.objNormals (\(Wavefront.Normal x y z) -> V3 x y z) (VertexAttribute "normal" nbo 1 3 GL_FLOAT GL_FALSE 0 nullPtr)
  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER GL_STATIC_DRAW (indexData :: [V3 GLushort])
  pure
    (DrawModel
      {vaoName = vao
      ,indexCount = fromIntegral $ length indexData * 3
      ,primitiveType = GL_TRIANGLES
      ,elementType = GL_UNSIGNED_SHORT
      ,textureName = Nothing
      }
    ,ModelData
      {modelVerticies = pos
      ,modelTexCoords = Nothing
      ,modelNormals = mNor
      ,modelIndicies = indexData
      }
    )

-- | Load a DrawModel from a file path to its obj
initTexturedModel :: FilePath -> FilePath -> IO (DrawModel, ModelData)
initTexturedModel modelPath texturePath = do
  logInfo $ "Initializing textured model " ++ modelPath ++ "..."
  (vao,[vbo,tbo,nbo,ibo]) <- generateVertexNames ["Vertex","Texture coordinate","Normal","Index"]
  textureId <- loadTexture texturePath
  logInfo "Binding VAO..."
  glBindVertexArray vao

  logInfo $ "Loading model " ++ modelPath ++ "..."
  model <- Wavefront.fromFile modelPath >>= \case
    Right m -> pure m
    Left e -> error e

  logInfo $ "Buffering vertex attributes for " ++ modelPath ++ "..."
  Just pos <- loadVertexAttr @(V3 GLfloat) model Wavefront.objLocations (\(Wavefront.Location x y z _w) -> V3 x y z) (VertexAttribute "position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr)
  Just tex <- loadVertexAttr @(V2 GLfloat) model Wavefront.objTexCoords (\(Wavefront.TexCoord r s _t) -> V2 r s) (VertexAttribute "texture coordinate" tbo 1 2 GL_FLOAT GL_FALSE 0 nullPtr)
  mNor <- loadVertexAttr @(V3 GLfloat) model Wavefront.objNormals (\(Wavefront.Normal x y z) -> V3 x y z) (VertexAttribute "normal" nbo 2 3 GL_FLOAT GL_FALSE 0 nullPtr)

  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER GL_STATIC_DRAW (indexData :: [V3 GLushort])
  pure
    (DrawModel
      {vaoName = vao
      ,indexCount = fromIntegral $ length indexData * 3
      ,primitiveType = GL_TRIANGLES
      ,elementType = GL_UNSIGNED_SHORT
      ,textureName = Just textureId
      }
    ,ModelData
      {modelVerticies = pos
      ,modelTexCoords = Just tex
      ,modelNormals = mNor
      ,modelIndicies = indexData
      }
    )

initLines :: [V3 GLfloat] -> [GLushort] -> IO DrawModel
initLines verts indexData = do
  (vao,[vbo,ibo]) <- generateVertexNames ["Vertex","Index"]
  logInfo "Binding VAO..."
  glBindVertexArray vao

  let lineAttr@VertexAttribute{..} = VertexAttribute "line vertex position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr

  bufferGLData attributeName attributeObjectName GL_ARRAY_BUFFER GL_STATIC_DRAW verts
  initGLAttr lineAttr

  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER GL_STATIC_DRAW indexData
  pure DrawModel
    {vaoName = vao
    ,indexCount = fromIntegral $ length indexData
    ,primitiveType = GL_LINES
    ,elementType = GL_UNSIGNED_SHORT
    ,textureName = Nothing
    }

generateVertexNames :: [String] -> IO (GLuint,[GLuint])
generateVertexNames bufs = do
  logInfo "Generating Vertex Array Object Name..."
  vao <- alloca $ \namePtr -> do
    glGenVertexArrays 1 namePtr
    peek namePtr
  logInfo $ "Vertex Array Object: " ++ show vao

  logInfo "Generating Vertex Buffer Names..."
  bufObjs <- alloca $ \namePtr -> glGenBuffers (fromIntegral $ length bufs) namePtr *> peekArray (length bufs) namePtr
  sequence_ $ zipWith (\n b -> logInfo $ "  " ++ n ++ " buffer: " ++ show b) bufs bufObjs
  pure (vao,bufObjs)
  

loadVertexAttr :: (Show q, Storable q) => Wavefront.WavefrontOBJ -> (Wavefront.WavefrontOBJ -> Vector.Vector e) -> (e -> q) -> VertexAttribute -> IO (Maybe [q])
loadVertexAttr model exElems toVec attr@VertexAttribute{..} = do
  let vecs = fmap toVec . Vector.toList . exElems $ model
  if null vecs
    then pure Nothing
    else do
      bufferGLData attributeName attributeObjectName GL_ARRAY_BUFFER GL_STATIC_DRAW vecs
      initGLAttr attr
      pure (Just vecs)

bufferGLData :: forall a. (Show a,Storable a) => String -> GLuint -> GLenum -> GLenum -> [a] -> IO ()
bufferGLData name bufferName bufferType bufferHint bufferData = do
  logInfo $ name ++ " data is:"
  mapM_ logInfo $ take 3 (map show bufferData) ++ ["..."] ++ drop (length bufferData - 3) (map show bufferData)
  logInfo $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logInfo $ "Buffering " ++ name ++ " data..."
  withArray bufferData $ \bufferDataArray -> do
    glBufferData bufferType (fromIntegral $ length bufferData * sizeOf (undefined :: a)) bufferDataArray bufferHint

setupEmptyBuffer :: String -> GLuint -> GLenum -> GLsizeiptr -> GLenum -> IO ()
setupEmptyBuffer name bufferName bufferType bufferSize bufferHint = do
  logInfo $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logInfo $ "Initializing " ++ name ++ " buffer with " ++ show bufferSize ++ " bytes of empty data"
  glBufferData bufferType bufferSize nullPtr bufferHint

initGLAttr :: VertexAttribute -> IO ()
initGLAttr (VertexAttribute{..}) = do
  logInfo $ "Enabling vertex " ++ attributeName ++ " attribute..."
  glEnableVertexAttribArray attributeIndex
  logInfo $ "Binding buffer for vertex " ++ attributeName ++ " attribute..."
  glBindBuffer GL_ARRAY_BUFFER attributeObjectName
  logInfo $ "Defining vertex " ++ attributeName ++ " attribute attributes..."
  glVertexAttribPointer attributeIndex attributeSize attributeType attributeNormalized attributeStride attributeOffset

sendMatrix :: String -> GLint -> M44 GLfloat -> IO ()
sendMatrix matName matId mat = do
  logTick $ "Sending " ++ matName ++ " matrix..."
  logTick $ prettyMatrix $ mat
  with mat $ glUniformMatrix4fv matId 1 GL_TRUE . (castPtr :: Ptr (M44 GLfloat) -> Ptr GLfloat)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

runFreeType :: IO FT.FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


data FreeTypeCharacter = FreeTypeCharacter
  {characterTextureName :: GLuint
  ,characterSize :: (FT.FT_Int,FT.FT_Int)
  ,characterBearing :: (FT.FT_Int,FT.FT_Int)
  ,characterAdvance :: FT.FT_Int -- ^ The advance in truncated full pixels (not 1/64ths)
  }

{-# NOINLINE globalFTCache #-}
globalFTCache :: IORef (Map.Map (Char,Int) FreeTypeCharacter)
globalFTCache = unsafePerformIO $ newIORef Map.empty

-- Load a FreeType character
-- Adapted from: http://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html
loadCharacter :: FilePath -> Int -> Char -> IO FreeTypeCharacter
loadCharacter path px char = Map.lookup (char,px) <$> readIORef globalFTCache >>= \case
  Just ftc -> pure ftc
  Nothing -> do
    ft <- alloca $ \p -> do
      runFreeType $ FT.ft_Init_FreeType p
      peek p
    logInfo $ "Initialized FreeType"

    -- Get the font face
    ff <- withCString path $ \str -> alloca $ \ptr -> do
      runFreeType $ FT.ft_New_Face ft str 0 ptr
      peek ptr
    logInfo $ "Got font face " ++ show path

    -- Set the size
    runFreeType $ FT.ft_Set_Pixel_Sizes ff (fromIntegral px) 0
    logInfo $ "Set size " ++ show px ++ " for " ++ show char

    glPixelStorei GL_UNPACK_ALIGNMENT 1

    -- Get the unicode character
    unicodeIndex <- FT.ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
    logInfo $ "Char " ++ show char ++ " has unicode index " ++ show unicodeIndex

    -- Load the unicode character
    runFreeType $ FT.ft_Load_Glyph ff unicodeIndex 0
    logInfo $ "Loaded unicode " ++ show char

    slot <- peek $ FT.glyph ff
    logInfo $ "Got slot for " ++ show char

    runFreeType $ FT.ft_Render_Glyph slot FT.ft_RENDER_MODE_NORMAL
    logInfo $ "Rendered " ++ show char ++ " into slot"

    -- Get the char bitmap.
    bmp <- peek $ FT.bitmap slot
    let
      w = FT.width bmp
      h = FT.rows bmp
    logInfo $ "The bitmap is " ++ show h ++ " rows of width " ++ show w

    logInfo "Generating texture name..."
    textureId <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
    logInfo "Binding texture..."
    glBindTexture GL_TEXTURE_2D textureId

    logInfo "Sending glyph texture to GL..."
    glTexImage2D
      GL_TEXTURE_2D
      0 -- Mipmap level (0 full res)
      (fromIntegral GL_RED) -- internal format
      (fromIntegral w)
      (fromIntegral h)
      0 -- This is 0 because OpenGL. L
      GL_RED -- stored format
      GL_UNSIGNED_BYTE -- size of color component
      (FT.buffer bmp)
         
    logInfo "Configuring mipmaps..."
    logInfo "  Magnify using linear filtering"
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
    logInfo "  Minify using linear blending and filtering"
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
    logInfo "  Wrap by repeating and clamping to edge"
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_CLAMP_TO_EDGE)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_CLAMP_TO_EDGE)
    logInfo "Generating mipmaps..."
    glGenerateMipmap GL_TEXTURE_2D
    advance <- FT.x <$> peek (FT.advance slot)
    bearing <- (,) <$> peek (FT.bitmap_left slot) <*> peek (FT.bitmap_top slot)
    let
      ftc = FreeTypeCharacter
        {characterTextureName = textureId
        ,characterSize = (w,h)
        ,characterBearing = bearing
        ,characterAdvance = fromIntegral $ shiftR advance 6
        }
    modifyIORef globalFTCache $ Map.insert (char,px) ftc
    pure ftc

-- | Generate the DrawInstructions for displaying the given string at the given location
renderString :: (FT.FT_Int,FT.FT_Int) -> String -> IO DrawInstructions
renderString origin str = do
  let size = 48
  (vao,[vbo]) <- generateVertexNames ["Vertex"]
  glBindVertexArray vao
  logInfo "Preloading string vbo with empty buffer..."
  let bufferSize = fromIntegral $ 4 * 6 * sizeOf (undefined :: GLfloat)
  setupEmptyBuffer "vertex/texture coord" vbo GL_ARRAY_BUFFER bufferSize GL_DYNAMIC_DRAW
  let vertAttr@VertexAttribute{..} = VertexAttribute "vertex/texture coord" vbo 0 4 GL_FLOAT GL_FALSE 0 nullPtr
  initGLAttr vertAttr
  chars <- forM str $ loadCharacter "/usr/share/fonts/TTF/DejaVuSans.ttf" size
  -- Each character is moved forward by the characterAdvance of the previous character
  let offsets = scanl (\c n -> c + (characterAdvance n)) 0 chars
  pure . DrawDirectly $ do
    glBindVertexArray vao
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    sequence_ . (\f -> zipWith f chars offsets) $ \FreeTypeCharacter{..} offset -> do
      let
        xorg = fromIntegral $ fst origin + offset + fst characterBearing
        yorg = fromIntegral $ snd origin - (snd characterSize - snd characterBearing)
        w = fromIntegral $ fst characterSize
        h = fromIntegral $ snd characterSize
        verts =
          [V4 xorg (yorg + h) 0 0
          ,V4 xorg yorg 0 1
          ,V4 (xorg + w) yorg 1 1
          ,V4 xorg (yorg + h) 0 0
          ,V4 (xorg + w) yorg 1 1
          ,V4 (xorg + w) (yorg + h) 1 0
          ]
      glBindTexture GL_TEXTURE_2D characterTextureName
      glBindBuffer GL_ARRAY_BUFFER vbo
      withArray (verts :: [V4 GLfloat]) $ glBufferSubData GL_ARRAY_BUFFER 0 bufferSize
      glDrawArrays GL_TRIANGLES 0 6
    glDisable GL_BLEND
