#ifndef Rift_h
#define Rift_h

extern "C" bool initRift();

extern "C" void clearRift();

extern "C" void readOrientation(float *roll, float *pitch, float *yaw);

extern "C" void readOrientationQ(float *x, float *y, float *z, float *w);

extern "C" void readPredictedOrientation(float *roll, float *pitch, float *yaw);

extern "C" void readPredictedOrientationQ(float *x, float *y, float *z, float *w);

extern "C" void readAcceleration(float *x, float *y, float *z);

extern "C" void populateProductInfo
( char *DisplayDeviceName,
  char *ProductName,
  char *Manufacturer,
  unsigned *Version);

extern "C" void populateScreenInfo
( unsigned *HResolution,
  unsigned *VResolution,
  float *HScreenSize,
  float *VScreenSize,
  float *VScreenCenter,
  float *EyeToScreenDistance,
  float *LensSeparationDistance,
  float *InterpupillaryDistance);

extern "C" void populateDistortionKInfo
( float *DistortionK0,
  float *DistortionK1,
  float *DistortionK2,
  float *DistortionK3);

extern "C" void populateChromaAbCorrectionInfo
( float *ChromaAbCorrection0,
  float *ChromaAbCorrection1,
  float *ChromaAbCorrection2,
  float *ChromaAbCorrection3);

#endif
