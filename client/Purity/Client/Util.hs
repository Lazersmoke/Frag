module Purity.Client.Util where

import Purity.Client.Data

import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL

plog :: LogLevel -> String -> IO ()
plog level message = case level of
  Log -> putStrLn $ "[Purity] " ++ message
  Warn -> putStrLn $ "[Purity][Warning] " ++ message
  Error -> putStrLn $ "[Purity][ERROR] " ++ message

rectangle2D :: (Double, Double) -> (Double,Double) -> IO ()
rectangle2D (x1,y1) (x2,y2) = do
  GL.vertex (GL.Vertex2 x1 y1 :: GL.Vertex2 GL.GLdouble)
  GL.vertex (GL.Vertex2 x2 y1 :: GL.Vertex2 GL.GLdouble)
  GL.vertex (GL.Vertex2 x2 y2 :: GL.Vertex2 GL.GLdouble)
  GL.vertex (GL.Vertex2 x1 y2 :: GL.Vertex2 GL.GLdouble)

renderPlane :: RenderPlane -> IO ()
renderPlane (RenderPlane a b c) = triangle3D a b c

triangle3D :: Vector -> Vector -> Vector -> IO ()
triangle3D a b c = forM_ [a,b,c] $ GL.vertex . vec2vert
 
vec2vert :: Vector -> GL.Vertex3 GL.GLdouble
vec2vert (Vector x y z) = GL.Vertex3 x y z
