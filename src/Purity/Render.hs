{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Purity.Render where

import Linear
import Control.Monad
import Graphics.GL.Core45
import qualified Codec.Wavefront as Wavefront
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Codec.Picture as Juicy
import Data.Void
import Foreign hiding (rotate)

import Purity.Data

data RenderModel = RenderModel
  {indexCount :: GLsizei
  ,vaoName :: GLuint
  }

drawModel :: RenderModel -> IO ()
drawModel rm = do
  logTag "Binding VAO..."
  glBindVertexArray (vaoName rm)
  logTag "Drawing triangles..."
  glDrawElements GL_TRIANGLES (indexCount rm) GL_UNSIGNED_SHORT nullPtr
  where
    logTag = logStrTag "drawModel"

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

-- | Load a RenderModel from a file path to its obj
initModel :: String -> IO RenderModel
initModel modelPath = do
  logTag "Generating Vertex Array Object Name..."
  vao <- alloca $ \namePtr -> do
    glGenVertexArrays 1 namePtr
    peek namePtr
  logTag $ "Vertex Array Object: " ++ show vao

  logTag "Generating Vertex Buffer Names..."
  [vbo,tbo,nbo,ibo] <- alloca $ \namePtr -> glGenBuffers 4 namePtr *> peekArray 4 namePtr
  logTag $ "Vertex buffer: " ++ show vbo
  logTag $ "Texture coordinate buffer: " ++ show tbo
  logTag $ "Normal buffer: " ++ show nbo
  logTag $ "Index buffer: " ++ show ibo

  logTag "Generating texture name..."
  textureId' <- alloca $ \namePtr -> glGenTextures 1 namePtr *> peek namePtr
  logTag "Binding texture..."
  glBindTexture GL_TEXTURE_2D textureId'

  logTag "Loading texture juicily from file..."
  Right dynImage <- Juicy.readPng "test.png"
  let testTexture = Juicy.convertRGB8 dynImage

  logTag "Sending texture to GL..."
  Storable.unsafeWith (Juicy.imageData testTexture) $ glTexImage2D
    GL_TEXTURE_2D
    0 -- Mipmap level (0 full res)
    (fromIntegral GL_RGB) -- internal format
    (fromIntegral $ Juicy.imageWidth testTexture)
    (fromIntegral $ Juicy.imageHeight testTexture)
    0 -- This is 0 because OpenGL. L
    GL_RGB -- stored format
    GL_UNSIGNED_BYTE -- size of color component

  logTag "Configuring mipmaps..."
  logTag "  Magnify using linear filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)
  logTag "  Minify using linear blending and filtering"
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  logTag "Generating mipmaps..."
  glGenerateMipmap GL_TEXTURE_2D

  logTag "Binding VAO..."
  glBindVertexArray vao

  logTag "Loading model..."
  model <- Wavefront.fromFile modelPath >>= \case
    Right m -> pure m
    Left e -> error e

  logTag "Buffering vertex attributes..."
  let vertexData = fmap (\(Wavefront.Location x y z _w) -> V3 x y z) . Vector.toList $ Wavefront.objLocations model
  bufferGLData "position" vbo GL_ARRAY_BUFFER (vertexData :: [V3 GLfloat])
  initGLAttr (VertexAttribute "position" vbo 0 3 GL_FLOAT GL_FALSE 0 nullPtr)

  let textureData = fmap (\(Wavefront.TexCoord r s _t) -> V2 r s) . Vector.toList . Wavefront.objTexCoords $ model
  when (not . null $ textureData) $ do
    bufferGLData "texture coordinate" tbo GL_ARRAY_BUFFER (textureData :: [V2 GLfloat])
    initGLAttr (VertexAttribute "texture coordinate" tbo 1 2 GL_FLOAT GL_FALSE 0 nullPtr)

  let normalData = fmap (\(Wavefront.Normal x y z) -> V3 x y z) . Vector.toList . Wavefront.objNormals $ model
  bufferGLData "normal" nbo GL_ARRAY_BUFFER (normalData :: [V3 GLfloat])
  initGLAttr (VertexAttribute "normal" nbo 2 3 GL_FLOAT GL_FALSE 0 nullPtr)

  let indexData = fmap (fmap (fromIntegral . subtract 1 . Wavefront.faceLocIndex) . (\(Wavefront.Face a b c _fs) -> V3 a b c) . Wavefront.elValue) . Vector.toList . Wavefront.objFaces $ model
  bufferGLData "index" ibo GL_ELEMENT_ARRAY_BUFFER (indexData :: [V3 GLushort])
  pure RenderModel {vaoName = vao,indexCount = fromIntegral $ length indexData * 3}
  where
    logTag = logStrTag "initModel"

bufferGLData :: forall a. (Show a,Storable a) => String -> GLuint -> GLenum -> [a] -> IO ()
bufferGLData name bufferName bufferType bufferData = do
  logTag $ name ++ " data is:"
  mapM_ (logStrTag "Data Buffering") $ take 3 (map show bufferData) ++ ["..."] ++ drop (length bufferData - 3) (map show bufferData)
  logTag $ "Binding " ++ name ++ " buffer..."
  glBindBuffer bufferType bufferName
  logTag $ "Buffering " ++ name ++ " data..."
  withArray bufferData $ \bufferDataArray -> do
    glBufferData bufferType (fromIntegral $ length bufferData * sizeOf (undefined :: a)) bufferDataArray GL_STATIC_DRAW
  where
    logTag = logStrTag "bufferGLData"

initGLAttr :: VertexAttribute -> IO ()
initGLAttr (VertexAttribute{..}) = do
  logTag $ "Enabling vertex " ++ attributeName ++ " attribute..."
  glEnableVertexAttribArray attributeIndex
  logTag $ "Binding buffer for vertex " ++ attributeName ++ " attribute..."
  glBindBuffer GL_ARRAY_BUFFER attributeObjectName
  logTag $ "Defining vertex " ++ attributeName ++ " attribute attributes..."
  glVertexAttribPointer attributeIndex attributeSize attributeType attributeNormalized attributeStride attributeOffset
  where
    logTag = logStrTag "initGLAttr"


