-- | Access Rift headtracking data and constants related to screen, distortion parameters, etc..

module Rift 
       ( RiftHandle
       , initRift, clearRift
       , orientation, orientationQ
       , predictedOrientation, predictedOrientationQ
       , acceleration
         
       , ProductInfo(..), productInfo
       , ScreenInfo(..), screenInfo
       , DistortionKInfo(..), distortionKInfoToVec4, distortionKInfo
       , ChromaAbCorrectionInfo(..), chromaAbCorrectionInfoToVec4, chromaAbCorrectionInfo
       ) where

import Control.Applicative

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Control.Monad.Cont

import Data.Vect.Floating
import Data.Vect.Floating.Util.Quaternion

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall unsafe "initRift" c_initRift :: IO Bool
foreign import ccall unsafe "clearRift" c_clearRift :: IO ()

foreign import ccall unsafe "readOrientation" c_readOrientation :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "readOrientationQ" c_readOrientationQ :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "readPredictedOrientation" c_readPredictedOrientation :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "readPredictedOrientationQ" c_readPredictedOrientationQ :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "readAcceleration" c_readAcceleration :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "populateProductInfo" c_populateProductInfo :: CString -> CString -> CString -> Ptr CUInt -> IO ()
foreign import ccall unsafe "populateScreenInfo" c_populateScreenInfo :: Ptr CUInt -> Ptr CUInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "populateDistortionKInfo" c_populateDistortionKInfo :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "populateChromaAbCorrectionInfo" c_populateChromaAbCorrectionInfo :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

-- | Handle to rift, constructor intentionally hidden, used to enforce that `initRift` was called before other Rift related functions.
data RiftHandle = R

-- | Error code for why call to `initRift` might have failed, not very descriptive yet.
data ErrorCode = InitializationFailed
               deriving Show

-- | Initializes the rift, returns ErrorCode if failure, returns a RiftHandle if success. Not safe to call twice without a call to `clearRift`.
initRift :: EitherT ErrorCode IO RiftHandle
initRift = do
  b <- liftIO c_initRift
  case b of
    True -> return R
    False -> left InitializationFailed
    
-- | Cleans up handle, frees memory, detaches from Rift, makes next call to `initRift` safe.
clearRift :: RiftHandle -> IO ()    
clearRift _ = c_clearRift

-- | Returns orientation of Rift in degrees starting at 0, from -pi to pi. 
-- Deconstructs to `Vec3` roll pitch yaw
orientation :: RiftHandle -> IO (Vec3 Float)
orientation _ = (`runContT` id) $ do
  roll <- ContT alloca
  pitch <- ContT alloca
  yaw <- ContT alloca
  liftIO $ do
    c_readOrientation roll pitch yaw
    return $ Vec3 
      <$> (fmap realToFrac . peek) roll 
      <*> (fmap realToFrac . peek) pitch 
      <*> (fmap realToFrac . peek) yaw

-- | Returns orientation of Rift in the UnitQuaternion, can apply as rotation with functions from `Data.Vect.Floating.Util.Quaternion` module in the <http://hackage.haskell.org/package/vect-floating> package.
orientationQ :: RiftHandle -> IO (UnitQuaternion Float)
orientationQ _ = (`runContT` id) $ do
  x <- ContT alloca
  y <- ContT alloca
  z <- ContT alloca
  w <- ContT alloca
  liftIO $ do
    c_readOrientationQ x y z w
    return . fmap unsafeToU $ Vec4 
      <$> (fmap realToFrac . peek) x 
      <*> (fmap realToFrac . peek) y 
      <*> (fmap realToFrac . peek) z 
      <*> (fmap realToFrac . peek) w

-- | Same as `orientation`, however, factors in velocity from Rift to give a predicted orientation with a lookahead defaulted to 0.03 seconds.
predictedOrientation :: RiftHandle -> IO (Vec3 Float)
predictedOrientation _ = (`runContT` id) $ do
  roll <- ContT alloca
  pitch <- ContT alloca
  yaw <- ContT alloca
  liftIO $ do
    c_readPredictedOrientation roll pitch yaw
    return $ Vec3 
      <$> (fmap realToFrac . peek) roll 
      <*> (fmap realToFrac . peek) pitch 
      <*> (fmap realToFrac . peek) yaw

-- | Same as 'orientationQ`, however, factors in velocity from Rift to give a predicted orientation with a lookahead defaulted to 0.03 seconds.
predictedOrientationQ :: RiftHandle -> IO (UnitQuaternion Float)
predictedOrientationQ _ = (`runContT` id) $ do
  x <- ContT alloca
  y <- ContT alloca
  z <- ContT alloca
  w <- ContT alloca
  liftIO $ do
    c_readPredictedOrientationQ x y z w
    return . fmap unsafeToU $ Vec4 
      <$> (fmap realToFrac . peek) x 
      <*> (fmap realToFrac . peek) y 
      <*> (fmap realToFrac . peek) z 
      <*> (fmap realToFrac . peek) w

-- | Returns last absolute acceleration reading, in m/s^2. Deconstructs to `Vec3` x y z.
acceleration :: RiftHandle -> IO (Vec3 Float)
acceleration _ = (`runContT` id) $ do
  x <- ContT alloca
  y <- ContT alloca
  z <- ContT alloca
  liftIO $ do
    c_readAcceleration x y z
    return $ Vec3 
      <$> (fmap realToFrac . peek) x 
      <*> (fmap realToFrac . peek) y 
      <*> (fmap realToFrac . peek) z
    
{- Constants from Oculus Rift -}

-- | Info about product.
data ProductInfo = 
  ProductInfo 
  { displayDeviceName :: String
  , productName :: String
  , manufacturer :: String
  , version :: Int
  } deriving (Show)
             
-- | Grabs product info from Rift.
productInfo :: RiftHandle -> IO ProductInfo
productInfo _ = (`runContT` id) $ do
  displayDeviceName' <- ContT $ withCString (replicate 33 '0')
  productName' <- ContT $ withCString (replicate 33 '0')
  manufacturer' <- ContT $ withCString (replicate 33 '0')
  version' <- ContT alloca
  liftIO $ do
    c_populateProductInfo displayDeviceName' productName' manufacturer' version'
    return $ ProductInfo
      <$> peekCString displayDeviceName' 
      <*> peekCString productName' 
      <*> peekCString manufacturer' 
      <*> (fmap fromIntegral . peek) version'
    
-- | Info about screen, all distances are in meters.
data ScreenInfo =
  ScreenInfo
  { hResolution :: Int
  , vResolution :: Int
  , hScreenSize :: Float
  , vScreenSize :: Float
  , vScreenCenter :: Float
  , eyeToScreenDistance :: Float
  , lensSeparationDistance :: Float
  , interpupillaryDistance :: Float
  } deriving (Show)
             
-- | Grabs screen info from Rift.
screenInfo :: RiftHandle -> IO ScreenInfo
screenInfo _ = (`runContT` id) $ do
  hResolution' <- ContT alloca
  vResolution' <- ContT alloca
  hScreenSize' <- ContT alloca
  vScreenSize' <- ContT alloca
  vScreenCenter' <- ContT alloca
  eyeToScreenDistance' <- ContT alloca
  lensSeparationDistance' <- ContT alloca
  interpupillaryDistance' <- ContT alloca
  liftIO $ do
    c_populateScreenInfo
      hResolution'
      vResolution'
      hScreenSize'
      vScreenSize'
      vScreenCenter'
      eyeToScreenDistance'
      lensSeparationDistance'
      interpupillaryDistance'
    return $ ScreenInfo
      <$> (fmap fromIntegral . peek) hResolution'
      <*> (fmap fromIntegral . peek) vResolution'
      <*> (fmap realToFrac . peek) hScreenSize'
      <*> (fmap realToFrac . peek) vScreenSize'
      <*> (fmap realToFrac . peek) vScreenCenter'
      <*> (fmap realToFrac . peek) eyeToScreenDistance'
      <*> (fmap realToFrac . peek) lensSeparationDistance'
      <*> (fmap realToFrac . peek) interpupillaryDistance'
    
-- | Information about distortion correction parameters necessary to correct lenses in Rift.
data DistortionKInfo =
  DistortionKInfo 
  { distortionK0 :: Float
  , distortionK1 :: Float
  , distortionK2 :: Float
  , distortionK3 :: Float
  } deriving (Show)
             
-- | Converts distortion correction parameters to a 4D vector.
distortionKInfoToVec4 :: DistortionKInfo -> Vec4 Float
distortionKInfoToVec4 (DistortionKInfo c0 c1 c2 c3) = Vec4 c0 c1 c2 c3    
             
-- | Grabs distortion correction parameters from Rift.
distortionKInfo :: RiftHandle -> IO DistortionKInfo
distortionKInfo _ = (`runContT` id) $ do
  distortionK0' <- ContT alloca
  distortionK1' <- ContT alloca
  distortionK2' <- ContT alloca
  distortionK3' <- ContT alloca
  liftIO $ do
    c_populateDistortionKInfo 
      distortionK0'
      distortionK1'
      distortionK2'
      distortionK3'
    return $ DistortionKInfo
      <$> (fmap realToFrac . peek) distortionK0'
      <*> (fmap realToFrac . peek) distortionK1'
      <*> (fmap realToFrac . peek) distortionK2'
      <*> (fmap realToFrac . peek) distortionK3'
    
-- | Information about chromatic aberration correction parameters necessary to correct color changes due to lenses in Rift.
data ChromaAbCorrectionInfo =
  ChromaAbCorrectionInfo 
  { chromaAbCorrection0 :: Float
  , chromaAbCorrection1 :: Float
  , chromaAbCorrection2 :: Float
  , chromaAbCorrection3 :: Float
  } deriving (Show)
             
-- | Converts chromatic aberration correction parameters to a 4D vector.
chromaAbCorrectionInfoToVec4 :: ChromaAbCorrectionInfo -> Vec4 Float
chromaAbCorrectionInfoToVec4 (ChromaAbCorrectionInfo c0 c1 c2 c3) = Vec4 c0 c1 c2 c3    
             
-- | Grabs chromatic aberration correction parameters from Rift.
chromaAbCorrectionInfo :: RiftHandle -> IO ChromaAbCorrectionInfo
chromaAbCorrectionInfo _ = (`runContT` id) $ do
  chromaAbCorrection0' <- ContT alloca
  chromaAbCorrection1' <- ContT alloca
  chromaAbCorrection2' <- ContT alloca
  chromaAbCorrection3' <- ContT alloca
  liftIO $ do
    c_populateChromaAbCorrectionInfo 
      chromaAbCorrection0'
      chromaAbCorrection1'
      chromaAbCorrection2'
      chromaAbCorrection3'
    return $ ChromaAbCorrectionInfo
      <$> (fmap realToFrac . peek) chromaAbCorrection0'
      <*> (fmap realToFrac . peek) chromaAbCorrection1'
      <*> (fmap realToFrac . peek) chromaAbCorrection2'
      <*> (fmap realToFrac . peek) chromaAbCorrection3'
