{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException (..), catch)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.ByteString.Builder (doubleLE, int32LE, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Int
import Math.Spatial
import Math.Vectorize
import Models.Aircraft
import qualified Network.Socket as NS
import Network.Socket.ByteString (send)
import Physics.RigidBody
import Sim.Sim
import Prelude

publish :: NS.Socket -> Int32 -> AircraftState Double -> IO ()
publish sock message_index aircraft = do
  let bs :: B.ByteString
      bs =
        L.toStrict
          . toLazyByteString
          . mconcat
          $ int32LE message_index : (doubleLE <$> vectorize aircraft)
      exceptionHandler :: SomeException -> IO ()
      exceptionHandler _ = threadDelay 1000000 >> putStrLn "UnityEngine is not running!"
  catch (void $ send sock bs) exceptionHandler

newSocket :: IO NS.Socket
newSocket = do
  -- Configure Unity socket connection.
  serveraddr : _ <- NS.getAddrInfo Nothing (Just "127.0.0.1") (Just "8081")
  sock <- NS.socket (NS.addrFamily serveraddr) NS.Datagram NS.defaultProtocol
  NS.connect sock (NS.addrAddress serveraddr)
  pure sock

closeSocket :: NS.Socket -> IO ()
closeSocket = NS.close

main :: IO ()
main = do
  sock <- newSocket
  let iters = 10000
      f :: Int32 -> AircraftState Double -> IO ()
      f 0 _ = closeSocket sock
      f n state = do
        let state' = stepOde (V3T (V3 0 0 0)) (V3T (V3 0 0 0)) state
        publish sock (iters - n) state'
        threadDelay (1000 * 10)
        f (n - 1) state'
  f
    iters
    AircraftState
      { dsRigidBody =
          RigidBodyState
            { ds_r_n2b_n = V3T $ V3 0 0 (-10),
              ds_v_bn_b = V3T $ V3 0 0 0,
              dsEulerN2B = Euler 0 0 0,
              ds_w_bn_b = V3T (V3 0 0 0)
            }
      }
