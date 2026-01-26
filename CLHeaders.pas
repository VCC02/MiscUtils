{
    Copyright (C) 2025 VCC
    creation date: 13 Mar 2025
    initial release date: 13 Mar 2025

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit CLHeaders;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, ctypes;


type
  cl_int = cint32;
  cl_uint = cuint32;
  cl_bitfield = QWord;
  cl_ulong = cuint64;
  cl_bool = cl_uint;

  _cl_platform_id = record
  end;

  _cl_device_id = record
  end;

  _cl_context = record
  end;

  _cl_command_queue = record
  end;

  _cl_mem = record
  end;

  _cl_program = record
  end;

  _cl_kernel = record
  end;

  _cl_event = record
  end;

  cl_platform_id = ^_cl_platform_id;
  cl_device_id = ^_cl_device_id;
  cl_context = ^_cl_context;
  cl_command_queue = ^_cl_command_queue;
  cl_mem = ^_cl_mem;
  cl_program = ^_cl_program;
  cl_kernel = ^_cl_kernel;
  cl_event = ^_cl_event;

  cl_device_type = cl_bitfield;
  cl_command_queue_properties = cl_bitfield;
  cl_context_properties = PtrInt;
  cl_properties = cl_ulong; // PtrUInt ????   //so far, this doesn't like any type related to cl_queue_properties;    //set to pointer, because an array has to be passed here
  Tcl_queue_properties = cl_properties;
  cl_program_build_info = cl_uint;
  cl_kernel_work_group_info = cl_uint;
  cl_mem_flags = cl_bitfield;
  cl_platform_info = cl_uint;
  cl_command_queue_info = cl_uint;

  Pcl_platform_id = ^cl_platform_id;
  Pcl_device_id = ^cl_device_id;
  pcl_uint = ^cl_uint;
  Pcl_context_properties = ^cl_context_properties;
  Pcl_queue_properties = ^Tcl_queue_properties;
  Pcl_event = ^cl_event;

  cl_device_info = cl_uint;

  cl_platform_id_arr = array[0..0] of cl_platform_id;
  cl_device_id_arr = array[0..0] of cl_device_id;

  {$IFnDEF Windows}
    size_t = PtrUInt;
  {$ENDIF}

  TContextNotify = procedure(errinfo: PAnsiChar; private_info: Pointer; cb: csize_t; user_data: Pointer); {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}  //cb = size
  TProgramNotify = procedure(_program: cl_program; user_data: Pointer); {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}

  TclGetPlatformIDs = function(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclGetDeviceIDs = function(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_devices: pcl_uint): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateContext = function(properties: Pcl_context_properties; num_devices: cl_uint; devices: Pcl_device_id; pfn_notify: TContextNotify; user_data: Pointer; var errcode_ret: cl_int): cl_context; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateCommandQueue = function(context: cl_context; device: cl_device_id; properties: cl_command_queue_properties; var errcode_ret: cl_int): cl_command_queue; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateProgramWithSource = function(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var errcode_ret: cl_int): cl_program; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclBuildProgram = function(_program: cl_program; num_devices: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; pfn_notify: TProgramNotify; user_data: Pointer): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclGetProgramBuildInfo = function(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateKernel = function(_program: cl_program; kernel_name: PAnsiChar; var errcode_ret: cl_int): cl_kernel; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclGetKernelWorkGroupInfo = function(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateBuffer = function(context: cl_context; flags: cl_mem_flags; size: csize_t; host_ptr: Pointer; var errcode_ret: cl_int): cl_mem; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclEnqueueWriteBuffer = function(command_queue: cl_command_queue; buffer: cl_mem; blocking_write: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclEnqueueReadBuffer = function(command_queue: cl_command_queue; buffer: cl_mem; blocking_read: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclSetKernelArg = function(kernel: cl_kernel; arg_index: cl_uint; arg_size: csize_t; arg_value: Pointer): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclEnqueueNDRangeKernel = function(command_queue: cl_command_queue; kernel: cl_kernel; work_dim: cl_uint; global_work_offset, global_work_size, local_work_size: Pcsize_t; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclFinish = function(command_queue: cl_command_queue): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclReleaseMemObject = function(memobj: cl_mem): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclReleaseKernel = function(kernel: cl_kernel): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclReleaseProgram = function(_program: cl_program): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclReleaseCommandQueue = function(command_queue: cl_command_queue): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclReleaseContext = function(context: cl_context): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclCreateCommandQueueWithProperties = function(context: cl_context; device: cl_device_id; properties: Pcl_queue_properties; var errcode_ret: cl_int): cl_command_queue; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclGetCommandQueueInfo = function(command_queue: cl_command_queue; param_name: cl_command_queue_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}

  TclGetPlatformInfo = function(_platform: cl_platform_id; param_name: cl_platform_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
  TclGetDeviceInfo = function(device: cl_device_id; param_name: cl_device_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}

  TOpenCL = class
  private
    FclGetPlatformIDs: TclGetPlatformIDs;
    FclGetDeviceIDs: TclGetDeviceIDs;
    FclCreateContext: TclCreateContext;
    FclCreateCommandQueue: TclCreateCommandQueue;
    FclCreateProgramWithSource: TclCreateProgramWithSource;
    FclBuildProgram: TclBuildProgram;
    FclGetProgramBuildInfo: TclGetProgramBuildInfo;
    FclCreateKernel: TclCreateKernel;
    FclGetKernelWorkGroupInfo: TclGetKernelWorkGroupInfo;
    FclCreateBuffer: TclCreateBuffer;
    FclEnqueueWriteBuffer: TclEnqueueWriteBuffer;
    FclEnqueueReadBuffer: TclEnqueueReadBuffer;
    FclSetKernelArg: TclSetKernelArg;
    FclEnqueueNDRangeKernel: TclEnqueueNDRangeKernel;
    FclFinish: TclFinish;
    FclReleaseMemObject: TclReleaseMemObject;
    FclReleaseKernel: TclReleaseKernel;
    FclReleaseProgram: TclReleaseProgram;
    FclReleaseCommandQueue: TclReleaseCommandQueue;
    FclReleaseContext: TclReleaseContext;
    FclCreateCommandQueueWithProperties: TclCreateCommandQueueWithProperties;
    FclGetCommandQueueInfo: TclGetCommandQueueInfo;

    FclGetPlatformInfo: TclGetPlatformInfo;
    FclGetDeviceInfo: TclGetDeviceInfo;

    FDllHandle: THandle;
    FExpectedDllDir: string;
    FExpectedDllFileName: string; //OpenCL.dll, libOpenCL.so
  public
    function clGetPlatformIDs(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int;
    function clGetDeviceIDs(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_devices: pcl_uint): cl_int;
    function clCreateContext(properties: Pcl_context_properties; num_devices: cl_uint; devices: Pcl_device_id; pfn_notify: TContextNotify; user_data: Pointer; var errcode_ret: cl_int): cl_context;
    function clCreateCommandQueue(context: cl_context; device: cl_device_id; properties: cl_command_queue_properties; var errcode_ret: cl_int): cl_command_queue; deprecated; //Still available in OpenCL 1.2, deprecated by OpenCL 2.0. Please use clCreateCommandQueueWithProperties if newer version than 1.2.
    function clCreateProgramWithSource(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var errcode_ret: cl_int): cl_program;
    function clBuildProgram(_program: cl_program; num_devices: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; pfn_notify: TProgramNotify; user_data: Pointer): cl_int;
    function clGetProgramBuildInfo(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
    function clCreateKernel(_program: cl_program; kernel_name: PAnsiChar; var errcode_ret: cl_int): cl_kernel;
    function clGetKernelWorkGroupInfo(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
    function clCreateBuffer(context: cl_context; flags: cl_mem_flags; size: csize_t; host_ptr: Pointer; var errcode_ret: cl_int): cl_mem;
    function clEnqueueWriteBuffer(command_queue: cl_command_queue; buffer: cl_mem; blocking_write: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
    function clEnqueueReadBuffer(command_queue: cl_command_queue; buffer: cl_mem; blocking_read: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
    function clSetKernelArg(kernel: cl_kernel; arg_index: cl_uint; arg_size: csize_t; arg_value: Pointer): cl_int;
    function clEnqueueNDRangeKernel(command_queue: cl_command_queue; kernel: cl_kernel; work_dim: cl_uint; global_work_offset, global_work_size, local_work_size: Pcsize_t; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
    function clFinish(command_queue: cl_command_queue): cl_int;
    function clReleaseMemObject(memobj: cl_mem): cl_int;
    function clReleaseKernel(kernel: cl_kernel): cl_int;
    function clReleaseProgram(_program: cl_program): cl_int;
    function clReleaseCommandQueue(command_queue: cl_command_queue): cl_int;
    function clReleaseContext(context: cl_context): cl_int;
    function clCreateCommandQueueWithProperties(context: cl_context; device: cl_device_id; properties: Pcl_queue_properties; var errcode_ret: cl_int): cl_command_queue;
    function clGetCommandQueueInfo(command_queue: cl_command_queue; param_name: cl_command_queue_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;

    function clGetPlatformInfo(_platform: cl_platform_id; param_name: cl_platform_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
    function clGetDeviceInfo(device: cl_device_id; param_name: cl_device_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;

    constructor Create;
    destructor Destroy; override;
    function Loaded: Boolean;
    procedure LoadOpenCLLibrary; //Called by constructor, but can be manually called after changing the file location.
    function ExpectedDllLocation: string;  //full path, ExpectedDllDir + ExpectedDllName

    property ExpectedDllDir: string read FExpectedDllDir write FExpectedDllDir;
    property ExpectedDllFileName: string read FExpectedDllFileName write FExpectedDllFileName; //witout path
  end;


const
  CL_SUCCESS = 0;
  CL_DEVICE_NOT_FOUND = -1;
  CL_OUT_OF_RESOURCES = -5;
  CL_OUT_OF_HOST_MEMORY = -6;
  CL_BUILD_PROGRAM_FAILURE = -11;
  CL_INVALID_VALUE = -30;
  CL_INVALID_PLATFORM = -32;
  CL_INVALID_DEVICE = -33;
  CL_INVALID_CONTEXT = -34;
  CL_INVALID_QUEUE_PROPERTIES = -35;
  CL_INVALID_COMMAND_QUEUE = -36;
  CL_INVALID_HOST_PTR = -37;
  CL_INVALID_MEM_OBJECT = -38;
  CL_INVALID_BUILD_OPTIONS = 43;
  CL_INVALID_PROGRAM = -44;
  CL_INVALID_KERNEL_NAME = -46;
  CL_INVALID_KERNEL = -48;
  CL_INVALID_ARG_INDEX = -49;
  CL_INVALID_ARG_VALUE = -50;
  CL_INVALID_ARG_SIZE = -51;
  CL_INVALID_KERNEL_ARGS = -52;
  CL_INVALID_WORK_DIMENSION = -53;
  CL_INVALID_WORK_GROUP_SIZE = -54;
  CL_INVALID_WORK_ITEM_SIZE = -55;
  CL_INVALID_GLOBAL_OFFSET = -56;
  CL_INVALID_BUFFER_SIZE = -61;

  CL_DEVICE_TYPE_CPU = 2;
  CL_DEVICE_TYPE_GPU = 4;
  CL_PROGRAM_BUILD_LOG = $1183;
  CL_KERNEL_WORK_GROUP_SIZE = $11B0;
  CL_QUEUE_REFERENCE_COUNT = $1092;
  CL_QUEUE_PROPERTIES = $1093;
  CL_QUEUE_SIZE = $1094;
  CL_QUEUE_PROPERTIES_ARRAY = $1098; //OpenCL >= 3.0
  CL_QUEUE_DEVICE_DEFAULT = $1095; //OpenCL >= 2.1

  CL_FALSE = 0;
  CL_TRUE = 1;

  CL_MEM_READ_WRITE = 1;
  CL_MEM_WRITE_ONLY = 2;
  CL_MEM_READ_ONLY = 4;
  CL_MEM_USE_HOST_PTR = 8;
  CL_MEM_ALLOC_HOST_PTR = 16;
  CL_MEM_COPY_HOST_PTR = 32;

  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = 1;
  CL_QUEUE_PROFILING_ENABLE = 2;
  CL_QUEUE_ON_DEVICE = 4;
  CL_QUEUE_ON_DEVICE_DEFAULT = 8;

  CL_PLATFORM_PROFILE = $0900;
  CL_PLATFORM_VERSION = $0901;
  CL_PLATFORM_NAME = $0902;
  CL_PLATFORM_VENDOR = $0903;
  CL_PLATFORM_EXTENSIONS = $0904;

  CL_DEVICE_NAME = $102B;
  CL_DEVICE_VENDOR = $102C;
  CL_DEVICE_VERSION = $102D;
  CL_DEVICE_PROFILE = $102E;
  CL_DEVICE_PLATFORM_VERSION = $102F;
  CL_DEVICE_OPENCL_C_VERSION = $103D;
  CL_DEVICE_EXTENSIONS = $1030;

  CL_DEVICE_TYPE_INFO = $1000;
  CL_DEVICE_GLOBAL_MEM_SIZE = $101F;
  CL_DEVICE_IMAGE_SUPPORT = $1016;
  CL_DEVICE_LOCAL_MEM_SIZE = $1023;
  CL_DEVICE_AVAILABLE = $1027;
  CL_DEVICE_COMPILER_AVAILABLE = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES = $1029;
  CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE = $104F;
  CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE = $1050;

  //device errors
  CLK_SUCCESS = 0;
  CLK_JUST_ANOTHER_ENQUEUE_FAILURE = -10; //unknown error. TBD - returned by enqueue_kernel. Value not found in OpenCL headers.
  CLK_ENQUEUE_FAILURE = -101;
  CLK_INVALID_QUEUE = -102;
  CLK_INVALID_NDRANGE = -160;
  CLK_INVALID_EVENT_WAIT_LIST = -57;
  CLK_DEVICE_QUEUE_FULL = -161;
  CLK_INVALID_ARG_SIZE = -51;
  CLK_EVENT_ALLOCATION_FAILURE = -100;
  CLK_OUT_OF_RESOURCES = -5;


function CLErrorToStr(AError: cl_int): string;
function CLDeviceErrorToStr(AError: cl_int): string;


implementation


type
  TCLErrMsg = record
    Code: Integer;
    Msg: string;
  end;

const
  CCLHostErrMsg: array[0..26] of TCLErrMsg = (
    (Code: CL_SUCCESS; Msg: ''),
    (Code: CL_DEVICE_NOT_FOUND; Msg: 'CL_DEVICE_NOT_FOUND'),
    (Code: CL_OUT_OF_RESOURCES; Msg: 'CL_OUT_OF_RESOURCES'),
    (Code: CL_OUT_OF_HOST_MEMORY; Msg: 'CL_OUT_OF_HOST_MEMORY'),
    (Code: CL_BUILD_PROGRAM_FAILURE; Msg: 'CL_BUILD_PROGRAM_FAILURE'),
    (Code: CL_INVALID_VALUE; Msg: 'CL_INVALID_VALUE'),
    (Code: CL_INVALID_DEVICE; Msg: 'CL_INVALID_DEVICE'),
    (Code: CL_INVALID_PLATFORM; Msg: 'CL_INVALID_PLATFORM'),
    (Code: CL_INVALID_CONTEXT; Msg: 'CL_INVALID_CONTEXT'),
    (Code: CL_INVALID_QUEUE_PROPERTIES; Msg: 'CL_INVALID_QUEUE_PROPERTIES'),
    (Code: CL_INVALID_COMMAND_QUEUE; Msg: 'CL_INVALID_COMMAND_QUEUE'),  //This might happen in case of an AV or calling a function, on device, with a bad resource identifier. It is also returned when the kernel enters an infinite loop.
    (Code: CL_INVALID_HOST_PTR; Msg: 'CL_INVALID_HOST_PTR'),
    (Code: CL_INVALID_MEM_OBJECT; Msg: 'CL_INVALID_MEM_OBJECT'),
    (Code: CL_INVALID_BUILD_OPTIONS; Msg: 'CL_INVALID_BUILD_OPTIONS'),
    (Code: CL_INVALID_PROGRAM; Msg: 'CL_INVALID_PROGRAM'),
    (Code: CL_INVALID_KERNEL_NAME; Msg: 'CL_INVALID_KERNEL_NAME'),
    (Code: CL_INVALID_KERNEL; Msg: 'CL_INVALID_KERNEL'),
    (Code: CL_INVALID_ARG_INDEX; Msg: 'CL_INVALID_ARG_INDEX'),
    (Code: CL_INVALID_ARG_VALUE; Msg: 'CL_INVALID_ARG_VALUE'),
    (Code: CL_INVALID_ARG_SIZE; Msg: 'CL_INVALID_ARG_SIZE'),
    (Code: CL_INVALID_KERNEL_ARGS; Msg: 'CL_INVALID_KERNEL_ARGS'),
    (Code: CL_INVALID_WORK_DIMENSION; Msg: 'CL_INVALID_WORK_DIMENSION'),
    (Code: CL_INVALID_WORK_GROUP_SIZE; Msg: 'CL_INVALID_WORK_GROUP_SIZE'),
    (Code: CL_INVALID_WORK_ITEM_SIZE; Msg: 'CL_INVALID_WORK_ITEM_SIZE'),
    (Code: CL_INVALID_GLOBAL_OFFSET; Msg: 'CL_INVALID_GLOBAL_OFFSET'),
    (Code: CL_INVALID_BUFFER_SIZE; Msg: 'CL_INVALID_BUFFER_SIZE'),

    (Code: -9999; Msg: 'Bad OpenCL state. Please restart application. Or maybe release and reload OpenCL.') //Happens on clCreateContext if clReleaseProgram or clReleaseCommandQueue could not be called previously. Or if clEnqueueReadBuffer was called after clFinish errored with CL_INVALID_COMMAND_QUEUE. This error code might be nvidia specific. TBD.  May mean: "Illegal read or write to a buffer."
  );

  CCLDeviceErrMsg: array[0..9] of TCLErrMsg = (
    (Code: CLK_SUCCESS; Msg: ''),
    (Code: CLK_JUST_ANOTHER_ENQUEUE_FAILURE; Msg: 'CLK_JUST_ANOTHER_ENQUEUE_FAILURE'), //unknown error. TBD - returned by enqueue_kernel. Value not found in OpenCL headers.
    (Code: CLK_ENQUEUE_FAILURE; Msg: 'CLK_ENQUEUE_FAILURE'),
    (Code: CLK_INVALID_QUEUE; Msg: 'CLK_INVALID_QUEUE'),
    (Code: CLK_INVALID_NDRANGE; Msg: 'CLK_INVALID_NDRANGE'),
    (Code: CLK_INVALID_EVENT_WAIT_LIST; Msg: 'CLK_INVALID_EVENT_WAIT_LIST'),
    (Code: CLK_DEVICE_QUEUE_FULL; Msg: 'CLK_DEVICE_QUEUE_FULL'),
    (Code: CLK_INVALID_ARG_SIZE; Msg: 'CLK_INVALID_ARG_SIZE'),
    (Code: CLK_EVENT_ALLOCATION_FAILURE; Msg: 'CLK_EVENT_ALLOCATION_FAILURE'),
    (Code: CLK_OUT_OF_RESOURCES; Msg: 'CLK_OUT_OF_RESOURCES')
  );

function CLErrorToStr(AError: cl_int): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(CCLHostErrMsg) - 1 do
    if AError = CCLHostErrMsg[i].Code then
    begin
      Result := CCLHostErrMsg[i].Msg;
      Exit;
    end;

  Result := 'Unknown error: ' + IntToStr(AError);
end;


function CLDeviceErrorToStr(AError: cl_int): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(CCLDeviceErrMsg) - 1 do
    if AError = CCLDeviceErrMsg[i].Code then
    begin
      Result := CCLDeviceErrMsg[i].Msg;
      Exit;
    end;

  Result := 'Unknown error: ' + IntToStr(AError);
end;


function GetWindowsLocationForOpenCL: string; //ideally, this should call a windows function, to get the installation dir
begin
  Result := 'C:\Windows\System32'
end;


function GetLinuxLocationForOpenCL: string;
begin
  Result := '/usr/include/CL';
end;


constructor TOpenCL.Create;
begin
  inherited Create;

  FclGetPlatformIDs := nil;
  FclGetDeviceIDs := nil;
  FclCreateContext := nil;
  FclCreateCommandQueue := nil;
  FclCreateProgramWithSource := nil;
  FclBuildProgram := nil;
  FclGetProgramBuildInfo := nil;
  FclCreateKernel := nil;
  FclGetKernelWorkGroupInfo := nil;
  FclCreateBuffer := nil;
  FclEnqueueWriteBuffer := nil;
  FclEnqueueReadBuffer := nil;
  FclSetKernelArg := nil;
  FclEnqueueNDRangeKernel := nil;
  FclFinish := nil;
  FclReleaseMemObject := nil;
  FclReleaseKernel := nil;
  FclReleaseProgram := nil;
  FclReleaseCommandQueue := nil;
  FclReleaseContext := nil;
  FclCreateCommandQueueWithProperties := nil;
  FclGetCommandQueueInfo := nil;

  FclGetPlatformInfo := nil;
  FclGetDeviceInfo := nil;

  {$IFDEF Windows}
    FExpectedDllDir := GetWindowsLocationForOpenCL;
    FExpectedDllFileName := 'OpenCL.dll';
  {$ELSE}
    FExpectedDllDir := GetLinuxLocationForOpenCL;
    FExpectedDllFileName := 'libOpenCL.so';
  {$ENDIF}

  LoadOpenCLLibrary;
end;


destructor TOpenCL.Destroy;
begin
  if FDllHandle > 0 then
    FreeLibrary(FDllHandle);

  inherited Destroy;
end;


procedure TOpenCL.LoadOpenCLLibrary;
begin
  if FDllHandle > 0 then
    FreeLibrary(FDllHandle);

  FDllHandle := LoadLibrary(ExpectedDllLocation);
  if FDllHandle = 0 then
    Exit;

  FclGetPlatformIDs := GetProcAddress(FDllHandle, 'clGetPlatformIDs');
  FclGetDeviceIDs := GetProcAddress(FDllHandle, 'clGetDeviceIDs');
  FclCreateContext := GetProcAddress(FDllHandle, 'clCreateContext');
  FclCreateCommandQueue := GetProcAddress(FDllHandle, 'clCreateCommandQueue');
  FclCreateProgramWithSource := GetProcAddress(FDllHandle, 'clCreateProgramWithSource');
  FclBuildProgram := GetProcAddress(FDllHandle, 'clBuildProgram');
  FclGetProgramBuildInfo := GetProcAddress(FDllHandle, 'clGetProgramBuildInfo');
  FclCreateKernel := GetProcAddress(FDllHandle, 'clCreateKernel');
  FclGetKernelWorkGroupInfo := GetProcAddress(FDllHandle, 'clGetKernelWorkGroupInfo');
  FclCreateBuffer := GetProcAddress(FDllHandle, 'clCreateBuffer');
  FclEnqueueWriteBuffer := GetProcAddress(FDllHandle, 'clEnqueueWriteBuffer');
  FclEnqueueReadBuffer := GetProcAddress(FDllHandle, 'clEnqueueReadBuffer');
  FclSetKernelArg := GetProcAddress(FDllHandle, 'clSetKernelArg');
  FclEnqueueNDRangeKernel := GetProcAddress(FDllHandle, 'clEnqueueNDRangeKernel');
  FclFinish := GetProcAddress(FDllHandle, 'clFinish');
  FclReleaseMemObject := GetProcAddress(FDllHandle, 'clReleaseMemObject');
  FclReleaseKernel := GetProcAddress(FDllHandle, 'clReleaseKernel');
  FclReleaseProgram := GetProcAddress(FDllHandle, 'clReleaseProgram');
  FclReleaseCommandQueue := GetProcAddress(FDllHandle, 'clReleaseCommandQueue');
  FclReleaseContext := GetProcAddress(FDllHandle, 'clReleaseContext');
  FclCreateCommandQueueWithProperties := GetProcAddress(FDllHandle, 'clCreateCommandQueueWithProperties'); //this should return nil on older OpenCL  (e.g. < 2.0)
  FclGetCommandQueueInfo := GetProcAddress(FDllHandle, 'clGetCommandQueueInfo');

  FclGetPlatformInfo := GetProcAddress(FDllHandle, 'clGetPlatformInfo');
  FclGetDeviceInfo := GetProcAddress(FDllHandle, 'clGetDeviceInfo');
end;


function TOpenCL.Loaded: Boolean;
begin
  Result := FDllHandle > 0;
end;


function TOpenCL.ExpectedDllLocation: string;
begin
  Result := ExpectedDllDir + PathDelim + ExpectedDllFileName;
end;


function TOpenCL.clGetPlatformIDs(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int;
begin
  Result := FclGetPlatformIDs(num_entries, platforms, num_platforms);
end;


function TOpenCL.clGetDeviceIDs(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_devices: pcl_uint): cl_int;
begin
  Result := FclGetDeviceIDs(_platform, device_type, num_entries, devices, num_devices);
end;


function TOpenCL.clCreateContext(properties: Pcl_context_properties; num_devices: cl_uint; devices: Pcl_device_id; pfn_notify: TContextNotify; user_data: Pointer; var errcode_ret: cl_int): cl_context;
begin
  Result := FclCreateContext(properties, num_devices, devices, pfn_notify, user_data, errcode_ret);
end;


function TOpenCL.clCreateCommandQueue(context: cl_context; device: cl_device_id; properties: cl_command_queue_properties; var errcode_ret: cl_int): cl_command_queue;
begin
  Result := FclCreateCommandQueue(context, device, properties, errcode_ret);
end;


function TOpenCL.clCreateProgramWithSource(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var errcode_ret: cl_int): cl_program;
begin
  Result := FclCreateProgramWithSource(context, count, strings, lengths, errcode_ret);
end;


function TOpenCL.clBuildProgram(_program: cl_program; num_devices: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; pfn_notify: TProgramNotify; user_data: Pointer): cl_int;
begin
  Result := FclBuildProgram(_program, num_devices, device_list, options, pfn_notify, user_data);
end;


function TOpenCL.clGetProgramBuildInfo(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
begin
  Result := FclGetProgramBuildInfo(_program, device, param_name, param_value_size, param_value, param_value_size_ret);
end;


function TOpenCL.clCreateKernel(_program: cl_program; kernel_name: PAnsiChar; var errcode_ret: cl_int): cl_kernel;
begin
  Result := FclCreateKernel(_program, kernel_name, errcode_ret);
end;


function TOpenCL.clGetKernelWorkGroupInfo(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
begin
  Result := FclGetKernelWorkGroupInfo(kernel, device, param_name, param_value_size, param_value, param_value_size_ret);
end;


function TOpenCL.clCreateBuffer(context: cl_context; flags: cl_mem_flags; size: csize_t; host_ptr: Pointer; var errcode_ret: cl_int): cl_mem;
begin
  Result := FclCreateBuffer(context, flags, size, host_ptr, errcode_ret);
end;


function TOpenCL.clEnqueueWriteBuffer(command_queue: cl_command_queue; buffer: cl_mem; blocking_write: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
begin
  Result := FclEnqueueWriteBuffer(command_queue, buffer, blocking_write, offset, size, ptr, num_events_in_wait_list, event_wait_list, event);
end;


function TOpenCL.clEnqueueReadBuffer(command_queue: cl_command_queue; buffer: cl_mem; blocking_read: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
begin
  Result := FclEnqueueReadBuffer(command_queue, buffer, blocking_read, offset, size, ptr, num_events_in_wait_list, event_wait_list, event);
end;


function TOpenCL.clSetKernelArg(kernel: cl_kernel; arg_index: cl_uint; arg_size: csize_t; arg_value: Pointer): cl_int;
begin
  Result := FclSetKernelArg(kernel, arg_index, arg_size, arg_value);
end;


function TOpenCL.clEnqueueNDRangeKernel(command_queue: cl_command_queue; kernel: cl_kernel; work_dim: cl_uint; global_work_offset, global_work_size, local_work_size: Pcsize_t; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int;
begin
  Result := FclEnqueueNDRangeKernel(command_queue, kernel, work_dim, global_work_offset, global_work_size, local_work_size, num_events_in_wait_list, event_wait_list, event);
end;


function TOpenCL.clFinish(command_queue: cl_command_queue): cl_int;
begin
  Result := FclFinish(command_queue);
end;


function TOpenCL.clReleaseMemObject(memobj: cl_mem): cl_int;
begin
  Result := FclReleaseMemObject(memobj);
end;


function TOpenCL.clReleaseKernel(kernel: cl_kernel): cl_int;
begin
  Result := FclReleaseKernel(kernel);
end;


function TOpenCL.clReleaseProgram(_program: cl_program): cl_int;
begin
  Result := FclReleaseProgram(_program);
end;


function TOpenCL.clReleaseCommandQueue(command_queue: cl_command_queue): cl_int;
begin
  Result := FclReleaseCommandQueue(command_queue);
end;


function TOpenCL.clReleaseContext(context: cl_context): cl_int;
begin
  Result := FclReleaseContext(context);
end;


function TOpenCL.clCreateCommandQueueWithProperties(context: cl_context; device: cl_device_id; properties: Pcl_queue_properties; var errcode_ret: cl_int): cl_command_queue;
begin
  Result := FclCreateCommandQueueWithProperties(context, device, properties, errcode_ret);
end;


function TOpenCL.clGetCommandQueueInfo(command_queue: cl_command_queue; param_name: cl_command_queue_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
begin
  Result := FclGetCommandQueueInfo(command_queue, param_name, param_value_size, param_value, param_value_size_ret);
end;


function TOpenCL.clGetPlatformInfo(_platform: cl_platform_id; param_name: cl_platform_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
begin
  Result := FclGetPlatformInfo(_platform, param_name, param_value_size, param_value, param_value_size_ret);
end;


function TOpenCL.clGetDeviceInfo(device: cl_device_id; param_name: cl_device_info; param_value_size: size_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int;
begin
  Result := FclGetDeviceInfo(device, param_name, param_value_size, param_value, param_value_size_ret);
end;


end.

