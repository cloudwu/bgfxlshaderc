#define LUA_LIB

extern "C" {
#include <lua.h>
#include <lauxlib.h>
}

/*
// for C99 

#include <stdbool.h>

typedef struct glslopt_shader glslopt_shader;
typedef struct glslopt_ctx glslopt_ctx;
typedef enum glslopt_target glslopt_target;
typedef enum glslopt_shader_type glslopt_shader_type;
typedef enum glslopt_basic_type glslopt_basic_type;
typedef enum glslopt_precision glslopt_precision;
*/

#include "glsl_optimizer.h"

static int
loptimize(lua_State *L) {
	const char * source = luaL_checkstring(L, 1);
	const char * opt = luaL_checkstring(L, 2);
	enum glslopt_shader_type type = kGlslOptShaderVertex;
	enum glslopt_target target = kGlslTargetOpenGL;
	int i;
	for (i=0;opt[i];i++) {
		switch(opt[i]) {
		case 'f':
			type = kGlslOptShaderFragment;
			break;
		case 'c':
			type = kGlslOptShaderCompute;
			break;
		case 'v':
			type = kGlslOptShaderVertex;
			break;
		case 'M':
			target = kGlslTargetMetal;
			break;
		case '2':
			target = kGlslTargetOpenGLES20;
			break;
		case '3':
			target = kGlslTargetOpenGLES30;
			break;
		default:
			return luaL_error(L, "Unknown option %c", opt[i]);
		}
	}
	struct glslopt_ctx* ctx = glslopt_initialize(target);
	struct glslopt_shader* shader = glslopt_optimize(ctx, type, source, 0);

	if (!glslopt_get_status(shader) ) {
		// error
		const char* log = glslopt_get_log(shader);
		lua_pushboolean(L, 0);
		lua_pushstring(L, log);
	} else {
		const char* optimizedShader = glslopt_get_output(shader);
		lua_pushboolean(L, 1);
		lua_pushstring(L, optimizedShader);
	}

	glslopt_cleanup(ctx);
	return 2;
}

extern "C" int luaopen_glslopt(lua_State *L);

LUAMOD_API int
luaopen_glslopt(lua_State *L) {
	luaL_checkversion(L);
	luaL_Reg l[] = {
		{ "optimize", loptimize },
		{ NULL, NULL },
	};
	luaL_newlib(L, l);
	return 1;
}
