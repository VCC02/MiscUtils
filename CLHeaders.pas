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
  cl_queue_properties = cl_properties;
  cl_program_build_info = cl_uint;
  cl_kernel_work_group_info = cl_uint;
  cl_mem_flags = cl_bitfield;

  Pcl_platform_id = ^cl_platform_id;
  Pcl_device_id = ^cl_device_id;
  pcl_uint = ^cl_uint;
  Pcl_context_properties = ^cl_context_properties;
  Pcl_queue_properties = ^cl_queue_properties;
  Pcl_event = ^cl_event;

  TContextNotify = procedure(errinfo: PAnsiChar; private_info: Pointer; cb: csize_t; user_data: Pointer); stdcall;  //cb = size
  TProgramNotify = procedure(_program: cl_program; user_data: Pointer); stdcall;

  TclGetPlatformIDs = function(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int; stdcall;
  TclGetDeviceIDs = function(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_devices: pcl_uint): cl_int; stdcall;
  TclCreateContext = function(properties: Pcl_context_properties; num_devices: cl_uint; devices: Pcl_device_id; pfn_notify: TContextNotify; user_data: Pointer; var errcode_ret: cl_int): cl_context; stdcall;
  TclCreateCommandQueue = function(context: cl_context; device: cl_device_id; properties: cl_command_queue_properties; var errcode_ret: cl_int): cl_command_queue; stdcall;
  TclCreateProgramWithSource = function(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var errcode_ret: cl_int): cl_program; stdcall;
  TclBuildProgram = function(_program: cl_program; num_devices: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; pfn_notify: TProgramNotify; user_data: Pointer): cl_int; stdcall;
  TclGetProgramBuildInfo = function(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; stdcall;
  TclCreateKernel = function(_program: cl_program; kernel_name: PAnsiChar; var errcode_ret: cl_int): cl_kernel; stdcall;
  TclGetKernelWorkGroupInfo = function(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; stdcall;
  TclCreateBuffer = function(context: cl_context; flags: cl_mem_flags; size: csize_t; host_ptr: Pointer; var errcode_ret: cl_int): cl_mem; stdcall;
  TclEnqueueWriteBuffer = function(command_queue: cl_command_queue; buffer: cl_mem; blocking_write: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; stdcall;
  TclEnqueueReadBuffer = function(command_queue: cl_command_queue; buffer: cl_mem; blocking_read: cl_bool; offset, size: size_t; ptr: Pointer; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; stdcall;
  TclSetKernelArg = function(kernel: cl_kernel; arg_index: cl_uint; arg_size: csize_t; arg_value: Pointer): cl_int; stdcall;
  TclEnqueueNDRangeKernel = function(command_queue: cl_command_queue; kernel: cl_kernel; work_dim: cl_uint; global_work_offset, global_work_size, local_work_size: Pcsize_t; num_events_in_wait_list: cl_uint; event_wait_list, event: Pcl_event): cl_int; stdcall;
  TclFinish = function(command_queue: cl_command_queue): cl_int; stdcall;
  TclReleaseMemObject = function(memobj: cl_mem): cl_int; stdcall;
  TclReleaseKernel = function(kernel: cl_kernel): cl_int; stdcall;
  TclReleaseProgram = function(_program: cl_program): cl_int; stdcall;
  TclReleaseCommandQueue = function(command_queue: cl_command_queue): cl_int; stdcall;
  TclReleaseContext = function(context: cl_context): cl_int; stdcall;
  TclCreateCommandQueueWithProperties = function(context: cl_context; device: cl_device_id; properties: Pcl_queue_properties; var errcode_ret: cl_int): cl_command_queue; stdcall;

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

    FDllHandle: THandle;
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

    constructor Create;
    destructor Destroy; override;
    function Loaded: Boolean;
    function ExpectedDllLocation: string;
  end;

//var
//  clGetPlatformIDs

const
  CL_SUCCESS = 0;
  CL_OUT_OF_RESOURCES = -5;
  CL_OUT_OF_HOST_MEMORY = -6;
  CL_BUILD_PROGRAM_FAILURE = -11;
  CL_INVALID_CONTEXT = -34;
  CL_INVALID_QUEUE_PROPERTIES = -35;
  CL_INVALID_COMMAND_QUEUE = -36;
  CL_INVALID_BUILD_OPTIONS = 43;
  CL_INVALID_PROGRAM = 44;
  CL_INVALID_KERNEL_ARGS = -52;
  CL_INVALID_WORK_DIMENSION = -53;
  CL_INVALID_WORK_GROUP_SIZE = -54;
  CL_INVALID_WORK_ITEM_SIZE = -55;
  CL_INVALID_GLOBAL_OFFSET = -56;
  CL_INVALID_BUFFER_SIZE = -61;

  CL_DEVICE_TYPE_GPU = 4;
  CL_PROGRAM_BUILD_LOG = $1183;
  CL_KERNEL_WORK_GROUP_SIZE = $11B0;

  CL_FALSE = 0;
  CL_TRUE = 1;

  CL_MEM_READ_WRITE = 1;
  CL_MEM_WRITE_ONLY = 2;
  CL_MEM_READ_ONLY = 4;
  CL_MEM_USE_HOST_PTR = 8;
  CL_MEM_ALLOC_HOST_PTR = 16;
  CL_MEM_COPY_HOST_PTR = 32;

  CL_QUEUE_ON_DEVICE = 4;
  CL_QUEUE_ON_DEVICE_DEFAULT = 8;


function CLErrorToStr(AError: cl_int): string;


implementation


function CLErrorToStr(AError: cl_int): string;
begin
  case AError of
    CL_SUCCESS: Result := '';
    CL_OUT_OF_RESOURCES: Result := 'CL_OUT_OF_RESOURCES';
    CL_OUT_OF_HOST_MEMORY: Result := 'CL_OUT_OF_HOST_MEMORY';
    CL_BUILD_PROGRAM_FAILURE: Result := 'CL_BUILD_PROGRAM_FAILURE';
    CL_INVALID_CONTEXT: Result := 'CL_INVALID_CONTEXT';
    CL_INVALID_QUEUE_PROPERTIES: Result := 'CL_INVALID_QUEUE_PROPERTIES';
    CL_INVALID_COMMAND_QUEUE: Result := 'CL_INVALID_COMMAND_QUEUE';
    CL_INVALID_BUILD_OPTIONS : Result := 'CL_INVALID_BUILD_OPTIONS';
    CL_INVALID_PROGRAM: Result := 'CL_INVALID_PROGRAM';
    CL_INVALID_KERNEL_ARGS: Result := 'CL_INVALID_KERNEL_ARGS';
    CL_INVALID_WORK_DIMENSION: Result := 'CL_INVALID_WORK_DIMENSION';
    CL_INVALID_WORK_GROUP_SIZE: Result := 'CL_INVALID_WORK_GROUP_SIZE';
    CL_INVALID_WORK_ITEM_SIZE: Result := 'CL_INVALID_WORK_ITEM_SIZE';
    CL_INVALID_GLOBAL_OFFSET: Result := 'CL_INVALID_GLOBAL_OFFSET';
    CL_INVALID_BUFFER_SIZE: Result := 'CL_INVALID_BUFFER_SIZE';
  end;
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
end;


destructor TOpenCL.Destroy;
begin
  if FDllHandle > 0 then
    FreeLibrary(FDllHandle);

  inherited Destroy;
end;


function TOpenCL.Loaded: Boolean;
begin
  Result := FDllHandle > 0;
end;


function GetWindowsLocationForOpenCL: string; //ideally, this should call a windows function, to get the installation dir
begin
  Result := '';
  {$IFDEF CPU32}
    Result := 'C:\Windows\System32\'
  {$ENDIF}

  {$IFDEF CPUX64}
    Result := 'C:\Windows\SysWOW64\';
  {$ENDIF}

  //...define for Linux
end;


function TOpenCL.ExpectedDllLocation: string;
begin
  Result := GetWindowsLocationForOpenCL + 'OpenCL.dll';
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


function TOpenCL.clGetProgramBuildInfo(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; param_value_size: csize_t; param_value: Pointer; var param_value_size_ret: csize_t): cl_int; stdcall;
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

end.

