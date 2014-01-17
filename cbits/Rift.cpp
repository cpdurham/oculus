// ==========================================================================
// RiftDK - C++ wrapper for reading head tracker data with pure C 
// Provide functions to initialize the RiftDK, and to read tracker data.
// ==========================================================================
// Revision History, Last Change First:
// YYYY/MM/DD Author               Description
// ========== ==================== ==========================================
// 2013/04/22 Geekmaster           First release version.

#include "Rift.h"
#include "OVR.h"
using namespace OVR;

SensorFusion* pFusion;
Ptr<DeviceManager> pManager;
Ptr<HMDDevice> pHMD;
Ptr<SensorDevice> pSensor;
HMDInfo hmdInfo;

extern "C" bool initRift() {
    System::Init(Log::ConfigureDefaultLog(LogMask_All));
    pManager = *DeviceManager::Create();
    pHMD = *pManager->EnumerateDevices<HMDDevice>().CreateDevice();

    if(pHMD == NULL) {
      return false;
    }

    pSensor = *pHMD->GetSensor();
    pFusion = new SensorFusion();
    pFusion->AttachToSensor(pSensor);
    pHMD->GetDeviceInfo(&hmdInfo);
    return true;
}

extern "C" void clearRift() {
  pSensor.Clear();
  pHMD.Clear();
  pManager.Clear();

  delete pFusion;

  System::Destroy();
}

extern "C" void readOrientation(float *roll, float *pitch, float *yaw) {
  pFusion->GetOrientation().GetEulerAngles<Axis_Y, Axis_X, Axis_Z>(yaw, pitch, roll);
}

extern "C" void readOrientationQ(float *x, float *y, float *z, float *w) {
  Quatf quatf = pFusion->GetOrientation();
  *x = quatf.x; *y = quatf.y; *z = quatf.z; *w = quatf.w;
}

extern "C" void readPredictedOrientation(float *roll, float *pitch, float *yaw) {
  pFusion->GetPredictedOrientation().GetEulerAngles<Axis_Y, Axis_X, Axis_Z>(yaw, pitch, roll);
}

extern "C" void readPredictedOrientationQ(float *x, float *y, float *z, float *w) {
  Quatf quatf = pFusion->GetPredictedOrientation();
  *x = quatf.x; *y = quatf.y; *z = quatf.z; *w = quatf.w;
}

extern "C" void readAcceleration(float *x, float *y, float *z) {
  Vector3f acc = pFusion->GetAcceleration();
  *x = acc.x; *y = acc.y; *z = acc.z;
}

extern "C" void populateProductInfo
( char *DisplayDeviceName,
  char *ProductName,
  char *Manufacturer,
  unsigned *Version) {
  strncpy(DisplayDeviceName, hmdInfo.DisplayDeviceName,32);
  strncpy(ProductName, hmdInfo.ProductName,32);
  strncpy(Manufacturer, hmdInfo.Manufacturer,32);
  *Version = hmdInfo.Version;
}

extern "C" void populateScreenInfo
( unsigned *HResolution, 
  unsigned *VResolution, 
  float *HScreenSize, 
  float *VScreenSize, 
  float *VScreenCenter,
  float *EyeToScreenDistance,
  float *LensSeparationDistance,
  float *InterpupillaryDistance) {
  *HResolution = hmdInfo.HResolution;
  *VResolution = hmdInfo.VResolution;
  *HScreenSize = hmdInfo.HScreenSize;
  *VScreenSize = hmdInfo.VScreenSize;
  *EyeToScreenDistance = hmdInfo.EyeToScreenDistance;
  *LensSeparationDistance = hmdInfo.LensSeparationDistance;
  *InterpupillaryDistance = hmdInfo.InterpupillaryDistance;
}

extern "C" void populateDistortionKInfo
( float *DistortionK0,
  float *DistortionK1,
  float *DistortionK2,
  float *DistortionK3) {
  *DistortionK0 = hmdInfo.DistortionK[0];
  *DistortionK1 = hmdInfo.DistortionK[1];
  *DistortionK2 = hmdInfo.DistortionK[2];
  *DistortionK3 = hmdInfo.DistortionK[3];
}

extern "C" void populateChromaAbCorrectionInfo
( float *ChromaAbCorrection0,
  float *ChromaAbCorrection1,
  float *ChromaAbCorrection2,
  float *ChromaAbCorrection3) {
  *ChromaAbCorrection0 = hmdInfo.ChromaAbCorrection[0];
  *ChromaAbCorrection1 = hmdInfo.ChromaAbCorrection[1];
  *ChromaAbCorrection2 = hmdInfo.ChromaAbCorrection[2];
  *ChromaAbCorrection3 = hmdInfo.ChromaAbCorrection[3];
}
