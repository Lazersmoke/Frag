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
import Graphics.GL.Core45
import Control.Monad
import qualified Codec.Wavefront as Wavefront
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Codec.Picture as Juicy
import Data.Void
import Foreign hiding (rotate)

import Purity.Data

data RenderModel = RenderModel
  {loadedModel :: DrawModel
  ,modelVerticies :: [V3 GLfloat]
  ,modelTexCoords :: Maybe [V2 GLfloat]
  ,modelNormals :: Maybe [V3 GLfloat]
  ,modelIndicies :: [V3 GLushort]
  }

data DrawModel = DrawModel
  {indexCount :: GLsizei
  ,vaoName :: GLuint
  }

-- | The per-model information needed to render
data RenderSpec = RenderSpec
  {modelMatrix :: M44 GLfloat
  ,viewMatrix :: M44 GLfloat
  ,projMatrix :: M44 GLfloat
  ,lightPos :: V3 GLfloat
  ,modelToRender :: DrawModel
  }

-- | All the context that must be passed from OpenGL initialization to the renderer
data OpenGLInfo = OpenGLInfo
  {shaderProgram :: GLuint
  ,modelMatrixId :: GLint
  ,viewMatrixId :: GLint
  ,projectionMatrixId :: GLint
  ,lightLocationId :: GLint
  ,textureSamplerId :: GLint
  }

-- | Pretty(ish) print a matrix-like structure
prettyMatrix :: (Foldable f, Foldable g, Show a) => f (g a) -> String
prettyMatrix = (++"\n") . foldMap ((++"\n") . foldMap ((++" \t") . show))

drawModel :: OpenGLInfo -> RenderSpec -> IO ()
drawModel OpenGLInfo{..} RenderSpec{..} = do
  forM_ [("Model",modelMatrixId,modelMatrix),("View",viewMatrixId,viewMatrix),("Projection",projectionMatrixId,projMatrix)] $ \(matName,matId,mat) -> do
    logTick $ "Sending " ++ matName ++ " matrix..."
    logTick $ prettyMatrix $ mat
    with mat $ glUniformMatrix4fv matId 1 GL_TRUE . (castPtr :: Ptr (M44 GLfloat) -> Ptr GLfloat)

  -- Send light position
  logTick "Sending light position..."
  with lightPos $ glUniform3fv lightLocationId 1 . castPtr

  logTick "Unleashing texture sampler on unit 0..."
  glUniform1i textureSamplerId 0

  logTick "Binding VAO..."
  glBindVertexArray (vaoName modelToRender)
  logTick "Drawing triangles..."
  glDrawElements GL_TRIANGLES (indexCount modelToRender) GL_UNSIGNED_SHORT nullPtr

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

-- | Load a DrawModel from a file path to its obj
initModel :: String -> IO RenderModel
initModel modelPath = do
  logInfo "Generating Vertex Array Object Name..."
  vao <- alloca $ \namePtr -> do
    glGenVertexArrays 1 namePtr
    peek namePtr
  logInfo $ "Vertex Array Object: " ++ show vao

  logInfo "Generating Vertex Buffer Names..."
  [vbo,tbo,nbo,ibo] <- alloca $ \namePtr -> glGenBuffers 4 namePtr *> peekArray 4 namePtr
  logInfo $ "  Vertex buffer: " ++ show vbo
  logInfo $ "  Texture coordinate buffer: " ++ show tbo
  logInfo $ "  Normal buffer: " ++ show nbo
  logInfo $ "  Index buffer: " ++ show ibo

  logInfo "Generating texture name..."
  textureId' <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
  logInfo "Binding texture..."
  glBindTexture GL_TEXTURE_2D textureId'

  logInfo "Loading texture juicily from file..."
  Right dynImage <- Juicy.readPng "test.png"
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

  logInfo "Binding VAO..."
  glBindVertexArray vao

  logInfo "Loading model..."
  model <- Wavefront.fromFile modelPath >>= \case
    Right m -> pure m
    Left e -> error e

  logInfo "Buffering vertex attributes..."
  Just pos <- loadVertexAttr @(V3 GLfloat) model Wavefront.objLocations (\(Wavefront.Location x y z _w) -> V3 x y z) (VertexAttribute "position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr)
  mTex <- loadVertexAttr @(V2 GLfloat) model Wavefront.objTexCoords (\(Wavefront.TexCoord r s _t) -> V2 r s) (VertexAttribute "texture coordinate" tbo 1 2 GL_FLOAT GL_FALSE 0 nullPtr)
  mNor <- loadVertexAttr @(V3 GLfloat) model Wavefront.objNormals (\(Wavefront.Normal x y z) -> V3 x y z) (VertexAttribute "normal" nbo 2 3 GL_FLOAT GL_FALSE 0 nullPtr)

  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER (indexData :: [V3 GLushort])
  pure RenderModel 
    {loadedModel = DrawModel {vaoName = vao,indexCount = fromIntegral $ length indexData * 3}
    ,modelVerticies = pos
    ,modelTexCoords = mTex
    ,modelNormals = mNor
    ,modelIndicies = indexData
    }

loadVertexAttr :: (Show q, Storable q) => Wavefront.WavefrontOBJ -> (Wavefront.WavefrontOBJ -> Vector.Vector e) -> (e -> q) -> VertexAttribute -> IO (Maybe [q])
loadVertexAttr model exElems toVec attr@(VertexAttribute{..}) = do
  let vecs = fmap toVec . Vector.toList . exElems $ model
  if null vecs
    then pure Nothing
    else do
      bufferGLData attributeName attributeObjectName GL_ARRAY_BUFFER vecs
      initGLAttr attr
      pure (Just vecs)

bufferGLData :: forall a. (Show a,Storable a) => String -> GLuint -> GLenum -> [a] -> IO ()
bufferGLData name bufferName bufferType bufferData = do
  logInfo $ name ++ " data is:"
  mapM_ logInfo $ take 3 (map show bufferData) ++ ["..."] ++ drop (length bufferData - 3) (map show bufferData)
  logInfo $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logInfo $ "Buffering " ++ name ++ " data..."
  withArray bufferData $ \bufferDataArray -> do
    glBufferData bufferType (fromIntegral $ length bufferData * sizeOf (undefined :: a)) bufferDataArray GL_STATIC_DRAW

initGLAttr :: VertexAttribute -> IO ()
initGLAttr (VertexAttribute{..}) = do
  logInfo $ "Enabling vertex " ++ attributeName ++ " attribute..."
  glEnableVertexAttribArray attributeIndex
  logInfo $ "Binding buffer for vertex " ++ attributeName ++ " attribute..."
  glBindBuffer GL_ARRAY_BUFFER attributeObjectName
  logInfo $ "Defining vertex " ++ attributeName ++ " attribute attributes..."
  glVertexAttribPointer attributeIndex attributeSize attributeType attributeNormalized attributeStride attributeOffset


