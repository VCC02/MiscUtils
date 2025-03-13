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

  cl_platform_id = ^_cl_platform_id;
  cl_device_id = ^_cl_device_id;
  cl_context = ^_cl_context;
  cl_command_queue = ^_cl_command_queue;
  cl_mem = ^_cl_mem;
  cl_program = ^_cl_program;
  cl_kernel = ^_cl_kernel;

  cl_device_type = cl_bitfield;
  cl_command_queue_properties = cl_bitfield;
  cl_context_properties = PtrInt;
  cl_program_build_info = cl_uint;
  cl_kernel_work_group_info = cl_uint;

  Pcl_platform_id = ^cl_platform_id;
  Pcl_device_id = ^cl_device_id;
  pcl_uint = ^cl_uint;
  Pcl_context_properties = ^cl_context_properties;

  TContextNotify = procedure(Name: PAnsiChar; Data: Pointer; Size: csize_t; Data2: Pointer); stdcall;
  TProgramNotify = procedure(_program: cl_program; user_data: Pointer); stdcall;

  TclGetPlatformIDs = function(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int; stdcall;
  TclGetDeviceIDs = function(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_device: pcl_uint): cl_int; stdcall;
  TclCreateContext = function(properties: Pcl_context_properties; num_device: cl_uint; devices: Pcl_device_id; notify: TContextNotify; user_data: Pointer; var ErrCode: cl_int): cl_context; stdcall;
  TclCreateCommandQueue = function(context: cl_context; devices: cl_device_id; properties: cl_command_queue_properties; ErrCode: cl_int): cl_command_queue; stdcall;
  TclCreateProgramWithSource = function(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var ErrCode: cl_int): cl_program; stdcall;
  TclBuildProgram = function(_program: cl_program; num_device: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; notify: TProgramNotify; user_data: Pointer): cl_int; stdcall;
  TclGetProgramBuildInfo = function(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; value_size: csize_t; value: Pointer; var Size: csize_t): cl_int; stdcall;
  TclCreateKernel = function(_program: cl_program; kernel_name: PAnsiChar; var ErrCode: cl_int): cl_kernel; stdcall;
  TclGetKernelWorkGroupInfo = function(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; value_size: csize_t; value: Pointer; Size: pcsize_t): cl_int; stdcall;


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

    FDllHandle: THandle;
  public
    function clGetPlatformIDs(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int;
    function clGetDeviceIDs(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_device: pcl_uint): cl_int;
    function clCreateContext(properties: Pcl_context_properties; num_device: cl_uint; devices: Pcl_device_id; notify: TContextNotify; user_data: Pointer; var ErrCode: cl_int): cl_context;
    function clCreateCommandQueue(context: cl_context; devices: cl_device_id; properties: cl_command_queue_properties; ErrCode: cl_int): cl_command_queue;
    function clCreateProgramWithSource(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var ErrCode: cl_int): cl_program;
    function clBuildProgram(_program: cl_program; num_device: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; notify: TProgramNotify; user_data: Pointer): cl_int;
    function clGetProgramBuildInfo(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; value_size: csize_t; value: Pointer; var Size: csize_t): cl_int;
    function clCreateKernel(_program: cl_program; kernel_name: PAnsiChar; var ErrCode: cl_int): cl_kernel;
    function clGetKernelWorkGroupInfo(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; value_size: csize_t; value: Pointer; Size: pcsize_t): cl_int;


    constructor Create;
    destructor Destroy; override;
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
const
  {$IFDEF CPU32}
    COpenCLPath: string = 'C:\Windows\System32\OpenCL.dll';
  {$ENDIF}

  {$IFDEF CPUX64}
    COpenCLPath: string = 'C:\Windows\SysWOW64\OpenCL.dll';
  {$ENDIF}

  //...define for Linux
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

  FDllHandle := LoadLibrary(COpenCLPath);
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
end;


destructor TOpenCL.Destroy;
begin
  if FDllHandle > 0 then
    FreeLibrary(FDllHandle);

  inherited Destroy;
end;


function TOpenCL.clGetPlatformIDs(num_entries: cl_uint; platforms: Pcl_platform_id; num_platforms: pcl_uint): cl_int;
begin
  Result := FclGetPlatformIDs(num_entries, platforms, num_platforms);
end;


function TOpenCL.clGetDeviceIDs(_platform: cl_platform_id; device_type: cl_device_type; num_entries: cl_uint; devices: Pcl_device_id; num_device: pcl_uint): cl_int;
begin
  Result := FclGetDeviceIDs(_platform, device_type, num_entries, devices, num_device);
end;


function TOpenCL.clCreateContext(properties: Pcl_context_properties; num_device: cl_uint; devices: Pcl_device_id; notify: TContextNotify; user_data: Pointer; var ErrCode: cl_int): cl_context;
begin
  Result := FclCreateContext(properties, num_device, devices, notify, user_data, ErrCode);
end;


function TOpenCL.clCreateCommandQueue(context: cl_context; devices: cl_device_id; properties: cl_command_queue_properties; ErrCode: cl_int): cl_command_queue;
begin
  Result := FclCreateCommandQueue(context, devices, properties, ErrCode);
end;


function TOpenCL.clCreateProgramWithSource(context: cl_context; count: cl_uint; strings: PPAnsiChar; lengths: Pcsize_t; var ErrCode: cl_int): cl_program;
begin
  Result := FclCreateProgramWithSource(context, count, strings, lengths, ErrCode);
end;


function TOpenCL.clBuildProgram(_program: cl_program; num_device: cl_uint; device_list: Pcl_device_id; options: PAnsiChar; notify: TProgramNotify; user_data: Pointer): cl_int;
begin
  Result := FclBuildProgram(_program, num_device, device_list, options, notify, user_data);
end;


function TOpenCL.clGetProgramBuildInfo(_program: cl_program; device: cl_device_id; param_name: cl_program_build_info; value_size: csize_t; value: Pointer; var Size: csize_t): cl_int; stdcall;
begin
  Result := FclGetProgramBuildInfo(_program, device, param_name, value_size, value, Size);
end;


function TOpenCL.clCreateKernel(_program: cl_program; kernel_name: PAnsiChar; var ErrCode: cl_int): cl_kernel;
begin
  Result := FclCreateKernel(_program, kernel_name, ErrCode);
end;


function TOpenCL.clGetKernelWorkGroupInfo(kernel: cl_kernel; device: cl_device_id; param_name: cl_kernel_work_group_info; value_size: csize_t; value: Pointer; Size: pcsize_t): cl_int;
begin
  Result := FclGetKernelWorkGroupInfo(kernel, device, param_name, value_size, value, Size);
end;

end.

