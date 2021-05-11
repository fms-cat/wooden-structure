#pragma once

#define glAttachShader			((PFNGLATTACHSHADERPROC)		wglGetProcAddress("glAttachShader"))
#define glBindFramebuffer		((PFNGLBINDFRAMEBUFFERPROC)		wglGetProcAddress("glBindFramebuffer"))
#define glCompileShader			((PFNGLCOMPILESHADERPROC)		wglGetProcAddress("glCompileShader"))
#define glCreateProgram			((PFNGLCREATEPROGRAMPROC)		wglGetProcAddress("glCreateProgram"))
#define glCreateShader			((PFNGLCREATESHADERPROC)		wglGetProcAddress("glCreateShader"))
#define glCreateShaderProgramv	((PFNGLCREATESHADERPROGRAMVPROC)wglGetProcAddress("glCreateShaderProgramv"))
#define glDebugMessageCallback	((PFNGLDEBUGMESSAGECALLBACKPROC)wglGetProcAddress("glDebugMessageCallback"))
#define glDeleteProgram			((PFNGLDELETEPROGRAMPROC)		wglGetProcAddress("glDeleteProgram"))
#define glDeleteShader			((PFNGLDELETESHADERPROC)		wglGetProcAddress("glDeleteShader"))
#define glDrawBuffers			((PFNGLDRAWBUFFERSPROC)			wglGetProcAddress("glDrawBuffers"))
#define glFramebufferTexture	((PFNGLFRAMEBUFFERTEXTUREPROC)	wglGetProcAddress("glFramebufferTexture"))
#define glGenFramebuffers		((PFNGLGENFRAMEBUFFERSPROC)		wglGetProcAddress("glGenFramebuffers"))
#define glGetProgramInfoLog		((PFNGLGETPROGRAMINFOLOGPROC)	wglGetProcAddress("glGetProgramInfoLog"))
#define glGetProgramiv			((PFNGLGETPROGRAMIVPROC)		wglGetProcAddress("glGetProgramiv"))
#define glGetShaderInfoLog		((PFNGLGETSHADERINFOLOGPROC)	wglGetProcAddress("glGetShaderInfoLog"))
#define glGetShaderiv			((PFNGLGETSHADERIVPROC)			wglGetProcAddress("glGetShaderiv"))
#define glGetUniformLocation	((PFNGLGETUNIFORMLOCATIONPROC)	wglGetProcAddress("glGetUniformLocation"))
#define glLinkProgram			((PFNGLLINKPROGRAMPROC)			wglGetProcAddress("glLinkProgram"))
#define glShaderSource			((PFNGLSHADERSOURCEPROC)		wglGetProcAddress("glShaderSource"))
#define glUniform1i				((PFNGLUNIFORM1IPROC)			wglGetProcAddress("glUniform1i"))
#define glUniform2i				((PFNGLUNIFORM2IPROC)			wglGetProcAddress("glUniform2i"))
#define glUniform3i				((PFNGLUNIFORM3IPROC)			wglGetProcAddress("glUniform3i"))
#define glUniform4i				((PFNGLUNIFORM4IPROC)			wglGetProcAddress("glUniform4i"))
#define glUniform1f				((PFNGLUNIFORM1FPROC)			wglGetProcAddress("glUniform1f"))
#define glUniform2f				((PFNGLUNIFORM2FPROC)			wglGetProcAddress("glUniform2f"))
#define glUniform3f				((PFNGLUNIFORM3FPROC)			wglGetProcAddress("glUniform3f"))
#define glUniform4f				((PFNGLUNIFORM4FPROC)			wglGetProcAddress("glUniform4f"))
#define glUseProgram			((PFNGLUSEPROGRAMPROC)			wglGetProcAddress("glUseProgram"))