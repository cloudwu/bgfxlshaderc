LUA_INC = -I /usr/local/include
LUA_LIB = -L /usr/local/bin -llua53
GLSLOPT_INC = -I ../bgfx/3rdparty/glsl-optimizer/src/glsl
GLSLOPT_LIB = -L ../bgfx/.build/win64_mingw-gcc/bin/ -lglsl-optimizerRelease

glslopt.dll : glslopt.cpp
	g++ --shared -o $@ $^ $(LUA_INC) $(GLSLOPT_INC) $(GLSLOPT_LIB) $(LUA_LIB)

clean :
	rm -f glslopt.dll
