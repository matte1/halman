{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Vectorize
  ( Vectorize (..),
  )
where

class Vectorize g a where
  vectorize :: g a -> [a]
  devectorize :: [a] -> g a
