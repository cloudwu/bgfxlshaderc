local lcpp = require "lcpp"

-- The lua 5.3 version of bgfx shaderc
-- Base on bgfx commit 3708ed0d16be513529a35ecbbe8ff7fe406c09f6 (Fri Sep 29 21:22:46 2017)

local Preprocessor = {}

Preprocessor.__index = Preprocessor

local function get_lang(p)
	local ret = { d3d = 11 }
	if p then
		if p == "s_4_0_level" then
			ret.hlsl = 2
		elseif p == "s_3" then
			ret.hlsl = 3
			ret.d3d  = 9
		elseif p == "s_4" then
			ret.hlsl = 4
		elseif p == "s_5" then
			ret.hlsl = 5
		elseif p == "metal" then
			ret.metal = 1
		elseif p == "pssl" then
			ret.pssl = 1
		elseif p == "spirv" then
			ret.spirv = 1
		else
			ret.glsl = assert(math.tointeger(p))
		end
	else
		ret.essl = 2
	end

	return ret
end

function Preprocessor.new(profile, defines, raw)
	local self = {
		m_lang = get_lang(profile),
		m_default = {},
		m_define = defines or {},
		m_raw = raw,
	}
	if self.m_lang.essl then
		table.insert(self.m_default, "#define lowp\n#define mediump\n#define highp\n")
	end
	return setmetatable(self, Preprocessor)
end

function Preprocessor:setDefaultDefine(name)
	local pat = "#ifndef $\n#	define $ 0\n#endif // $\n"
	table.insert(self.m_default , (pat:gsub("%$", name)))
end

function Preprocessor:setDefine(define, v)
	self.m_define[define] = v or true
end

do
	local default_define = {
		"BX_PLATFORM_ANDROID",
		"BX_PLATFORM_EMSCRIPTEN",
		"BX_PLATFORM_IOS",
		"BX_PLATFORM_LINUX",
		"BX_PLATFORM_OSX",
		"BX_PLATFORM_PS4",
		"BX_PLATFORM_WINDOWS",
		"BX_PLATFORM_XBOXONE",

		"BGFX_SHADER_LANGUAGE_GLSL",
		"BGFX_SHADER_LANGUAGE_HLSL",
		"BGFX_SHADER_LANGUAGE_METAL",
		"BGFX_SHADER_LANGUAGE_PSSL",
		"BGFX_SHADER_LANGUAGE_SPIRV",

		"BGFX_SHADER_TYPE_COMPUTE",
		"BGFX_SHADER_TYPE_FRAGMENT",
		"BGFX_SHADER_TYPE_VERTEX",
	}

	function Preprocessor:setDefaultDefines()
		for _, v in ipairs(default_define) do
			self:setDefaultDefine(v)
		end
	end
end

do
	local shader_type = {
		c = "BGFX_SHADER_TYPE_COMPUTE",
		f = "BGFX_SHADER_TYPE_FRAGMENT",
		v = "BGFX_SHADER_TYPE_VERTEX",
	}

	function Preprocessor:defineShaderType(st)
		self:setDefine(assert(shader_type[st]),1)
		self.m_shaderType = st
	end
end

local shaderc = {}

function Preprocessor:definePlatform(platform)
	local lang = self.m_lang
	if platform == "android" then
		self:setDefine("BX_PLATFORM_ANDROID", 1)
		self:setDefine("BGFX_SHADER_LANGUAGE_GLSL", 1)
	elseif platform == "asm.js" then
		self:setDefine("BX_PLATFORM_EMSCRIPTEN" , 1)
		self:setDefine("BGFX_SHADER_LANGUAGE_GLSL", 1)
	elseif platform == "ios" then
		self:setDefine("BX_PLATFORM_IOS",1)
		self:setDefine("BGFX_SHADER_LANGUAGE_GLSL", 1)
	elseif platform == "linux" then
		self:setDefine("BX_PLATFORM_LINUX",1)
		if lang.spirv then
			self:setDefine("BGFX_SHADER_LANGUAGE_SPIRV",1)
		else
			self:setDefine("BGFX_SHADER_LANGUAGE_GLSL", lang.essl and 1 or lang.glsl)
		end
	elseif platform == "osx" then
		self:setDefine("BX_PLATFORM_OSX",1)
		self:setDefine("BGFX_SHADER_LANGUAGE_GLSL", lang.essl and 1 or lang.glsl)
		self:setDefine("BGFX_SHADER_LANGUAGE_METAL", lang.metal or 0)
	elseif platform == "windows" then
		self:setDefine("BX_PLATFORM_WINDOWS", 1)
		self:setDefine("BGFX_SHADER_LANGUAGE_HLSL", lang.hlsl or 0)
	elseif platform == "orbis" then
		self:setDefine("BX_PLATFORM_PS4",1)
		self:setDefine("BGFX_SHADER_LANGUAGE_PSSL",1)
		self:setDefine("lit","lit_reserved")
	end
end

local precision_keyword = {
	lowp = true,
	mediump = true,
	highp = true,
}

local interpolation_keyword = {
	flat = "nointerpolation",	-- for interpolationDx11
	smooth = "linear",
	noperspective = "noperspective",
	centroid = "centroid",
}

function Preprocessor:parseVaryingMap(varyingdef)
	local lang = self.m_lang
	local vmap = {}
	self.m_varying = vmap
	for line in varyingdef:gmatch "([^;]+);" do
		local name,semantics = line:match "([^:]+):([^:]+)"
		assert(name and semantics, "Invalid varying")
		local varying = {}
		local tmp = {}
		for v in name:gmatch "%S+" do
			table.insert(tmp, v)
		end
		if #tmp < 2 or #tmp > 4 then
			error ("invalid varying " .. name)
		end
		local v1,v2,v3,v4 = table.unpack(tmp)
		if precision_keyword[v1] then
			varying.precision = v1
			v1,v2,v3 = v2,v3,v4
		else
			varying.precision = ""
		end
		if interpolation_keyword[v1] then
			varying.interpolation = v1
			varying.interpolationdx = interpolation_keyword[v1]
			v1,v2 = v2,v3
		else
			varying.interpolation = ""
			varying.interpolationdx = ""
		end
		assert(v1 and v2 and v3 == nil, "Invalid varying")
		assert(vmap[v2] == nil, "varing redefined")
		varying.type = v1
		varying.name = v2
		vmap[v2] = varying

		local v1, v2 = semantics:match "^%s*([%w_]+)%s*(.*)"
		assert(v1, "varying need semantics")
		if lang.d3d == 9 and v1 == "BITANGENT" then
			v1 = "BINORMAL"
		end
		varying.semantics = v1
		if v2 ~= "" then
			assert(v2:sub(1,1) == "=")
		end
		varying.init = v2
	end
end

-- HashMurmur2A from bx, rewrite in lua

local MURMUR_M = 0x5bd1e995

local function mmix(hash, dword)
	dword = (dword * MURMUR_M) & 0xffffffff
	dword = dword ~ (dword >> 24)
	hash = ((hash * MURMUR_M) ~ (dword * MURMUR_M)) & 0xffffffff
	return hash
end

local function HashMurmur2A(str, seed)
	local len = #str
	local hash = seed or 0

	for i = 1, len-3, 4 do
		local dword = string.unpack("<i4",str, i)
		hash = mmix(hash, dword)
	end

	local tail_len = len % 4
	local tail = 0
	if tail_len > 0 then
		tail = string.unpack("<i" .. tail_len , str:sub( - tail_len))
	end

	hash = mmix(hash, tail)
	hash = mmix(hash, len)
	hash = ((hash ~ (hash >> 13)) * MURMUR_M) & 0xffffffff
	hash = hash ~ (hash >> 15)

	return hash
end

function Preprocessor:parseHeader(source)
	local index = 1
	while source:sub(index,index) == "$" do
		local eol = source:find("\n", index, true)
		local line = source:sub(index+1, eol-1)
		index = eol + 1
		local comment = line:find "//"
		if comment then
			line = line:sub(1, comment -1 )
		end
		local token = {}
		for v in line:gmatch "[^%s,]+" do
			table.insert(token, v)
		end
		local command = table.remove(token, 1)
		table.sort(token)
		if command == "raw" then
			self.m_raw = true
		elseif command == "input" or command == "output" then
			self["m_" .. command] = token
			self["m_" .. command .. "Hash"] = HashMurmur2A(table.concat(token))
		end
	end
	if index > 1 then
		return source:sub(index)
	else
		return source
	end
end

local BGFX_CHUNK_MAGIC = {
	c = "CSH\3",
	f = "FSH\5",
	v = "VSH\5",
}

function Preprocessor:genRaw(source)
	local r = {
		BGFX_CHUNK_MAGIC[self.m_shaderType],
		string.pack("<I4", (self.m_shaderType == 'f' and self.m_inputHash or self.m_outputHash) or 0),
		nil,
	}
	if self.m_lang.glsl then
		table.insert(r, string.pack("<hs4b", 0, source, 0))
	end
--[[
	elseif self.m_lang.pssl then
		-- todo: compile PSSL
		error("Not support PSSL")
	else
		-- todo: compile HLSL
		error("Not support HLSL")
	end
]]
	return table.concat(r)
end

local NOTGLSL_DEFINE =[[
#define lowp
#define mediump
#define highp
#define ivec2 int2
#define ivec3 int3
#define ivec4 int4
#define uvec2 uint2
#define uvec3 uint3
#define uvec4 uint4
#define vec2 float2
#define vec3 float3
#define vec4 float4
#define mat2 float2x2
#define mat3 float3x3
#define mat4 float4x4
]]


local function replace_main(source, args, enter_main, leave_main, return_type)
	local decl = table.concat(args, "\n\t,")
	if enter_main then
		enter_main = table.concat(enter_main, "\n\t")
	else
		enter_main = ""
	end
	if leave_main then
		leave_main = table.concat(leave_main, "\n\t")
	else
		leave_main = ""
	end
	local new_main = string.format("%s main ( %s ) {\n\t%s\n%%1\n\t%s\n}", return_type or "void", decl, enter_main, leave_main)
	local replace, n = source:gsub("void main%s*%(%s*%)%s*(%b{})", new_main)
	assert(n == 1, "Shader entry point 'void main()' is not found or more than once")

	return replace
end

local compute_main_parm = {
	{ "gl_LocalInvocationID" , "int3", "SV_GroupThreadID" },
	{ "gl_LocalInvocationIndex", "int", "SV_GroupIndex" },
	{ "gl_GlobalInvocationID", "int3", "SV_DispatchThreadID" },
	{ "gl_WorkGroupID", "int3", "SV_GroupID" },
}

function Preprocessor:preprocessComputeShader(source)
	local tmp = {}
	for _, v in ipairs(compute_main_parm) do
		if source:find(v[1], 1, true) then
			table.insert(tmp, string.format("%s %s : %s", v[2],v[1],v[3]))
		end
	end
	local source =  replace_main(source, not (lang.glsl or lang.essl or lang.metal) and NOTGLSL_DEFINE)
	table.insert(tmp, source)

	return table.concat(tmp, "\n")
end

local function varying_replace(self, what, result, pat1, pat2)
	local tokens = self[what]
	if not tokens then
		return
	end
	for _, token in ipairs(tokens) do
		local varying = self.m_varying[token]
		if varying then
			if pat2 then
				local name = varying.name
				local prefix = name:sub(1,2)
				if prefix == "a_" or prefix == "i_" then
					table.insert(result, (pat1:gsub("%$(%l+)", varying)))
				else
					table.insert(result, (pat2:gsub("%$(%l+)", varying)))
				end
			else
				table.insert(result, (pat1:gsub("%$(%l+)", varying)))
			end
		end
	end
end

function Preprocessor:processGLShader(source)
	local header = {}
	if not self.m_lang.essl then
		table.insert(header, "#define shadow2D(_sampler, _coord) bgfxShadow2D(_sampler, _coord).x")
		table.insert(header, "#define shadow2DProj(_sampler, _coord) bgfxShadow2DProj(_sampler, _coord).x")
	end
	varying_replace(self, "m_input", header,
		"attribute $precision $interpolation $type $name;",
		"$interpolation varying $precision $type $name;")
	varying_replace(self, "m_output", header, "$interpolation varying $precision $type $name;")
	table.insert(header, source)
	return table.concat(header, "\n")
end

function Preprocessor:processFHLShader(header, source)
	local lang = self.m_lang
	local enter_main = { "vec4 bgfx_VoidFrag = vec4_splat(0.0);" }

	local hasFragData = {}
	local numFragData = 0
	for i=1,8 do
		local key = "gl_FragData%[%s*" .. (i - 1) .. "%s*%]"
		if source:find(key) then
			hasFragData[i] = true
			numFragData = numFragData + 1
		end
	end
	if numFragData == 0  then
		table.insert(header, "#define gl_FragColor gl_FragData_0_")
		local hasFragColor = source:find("gl_FragColor",1,true)
		if hasFragColor or lang.d3d < 11 then
			hasFragData[1] = true
		end
		if lang.d3d < 11 and not hasFragColor then
			table.insert(enter_main, "gl_FragColor = bgfx_VoidFrag;")
		end
	end

	local hasFrontFacing = source:find("gl_FrontFacing",1,true)
	if hasFrontFacing then
		if lang.hlsl >= 3 then
			table.insert(header, "#define gl_FrontFacing (__vface <= 0.0)")
		else
			table.insert(header, "#define gl_FrontFacing false")
		end
	end

	local args = {}
	if lang.hlsl > 3 or lang.hlsl == 2 or source:find("gl_FragCoord",1,true) then
		table.insert(args, "vec4 gl_FragCoord : SV_POSITION")
	end
	varying_replace(self, "m_input", args, "$interpolationdx $type $name : $semantics")

	local maxRT = lang.d3d > 9 and 8 or 4
	for i = 1, maxRT do
		if hasFragData[i] then
			table.insert(args, string.format("out vec4 gl_FragData_%d_ : SV_TARGET%d"
				, i-1, i-1))
		end
	end
	source = source:gsub("gl_FragData%[%s*(%d)%s*%]", function(id)
			if tonumber(id) <= maxRT then
				return "gl_FragData_" .. id .. "_"
			else
				return "bgfx_VoidFrag"
			end
		end)

	if hasFrontFacing and lang.hlsl >= 3 then
		table.insert(args, "float __vface : VFACE")
	end

	if source:find("gl_PrimitiveID",1,true) then
		if lang.d3d <= 9 then
			error "gl_PrimitiveID builtin is not supported by this D3D9 HLSL."
		end
		table.insert(args, "uint gl_PrimitiveID : SV_PrimitiveID")
	end

	return replace_main(source, args, enter_main)
end

function Preprocessor:processVHLShader(header, source)
	local lang = self.m_lang
	table.insert(header,"struct Output {\n\tvec4 gl_Position : SV_POSITION;\n#define gl_Position _varying_.gl_Position")
	varying_replace(self, "m_output", header, "\t$type $name : $semantics;\n#define $name _varying_.$name")
	table.insert(header, "};")

	local args = {}
	varying_replace(self, "m_input", args, "$type $name : $semantics")
	if source:find("gl_VertexID", 1, true) then
		if lang.d3d <= 9 then
			error "gl_VertexID builtin is not supported by this D3D9 HLSL."
		end
		table.insert(args, "uint gl_VertexID : SV_VertexID")
	end
	if source:find("gl_InstanceID", 1, true) then
		if lang.d3d <= 9 then
			error "gl_InstanceID builtin is not supported by this D3D9 HLSL."
		end
		table.insert(args, "uint gl_InstanceID : SV_InstanceID")
	end

	local enter_main = { "Output _varying_;" }
	varying_replace(self, "m_output", enter_main, "$name$init;")

	local leave_main = {}

--	if lang.hlsl and lang.hlsl <= 3 then
--		table.insert(leave_main, "gl_Position.xy += u_viewTexel.xy * gl_Position.w;")
--	end

	table.insert(leave_main, "return _varying_;")

	return replace_main(source, args, enter_main, leave_main, "Output")
end

function Preprocessor:processHLShader(source)
	local lang = self.m_lang
	local header = { NOTGLSL_DEFINE }
	if lang.hlsl and lang.hlsl < 4 then
		table.insert(header, "#define centroid\n#define flat\n#define noperspective\n#define smooth")
	end
	if self.m_shaderType == 'f' then
		source = self:processFHLShader(header, source)
	elseif self.m_shaderType == 'v' then
		source = self:processVHLShader(header, source)
	else
		error "Invalid shaderType"
	end
	table.insert(header, source)
	return table.concat(header, "\n")
end

function Preprocessor:preprocessShader(source)
	local lang = self.m_lang
	if lang.glsl or lang.essl or lang.metal then
		return self:processGLShader(source)
	else
		return self:processHLShader(source)
	end
end

function Preprocessor:parseSource(source_list)
	local source = assert(source_list[1])
	if source:sub(1,3) == "\xef\xbb\xbf" then
		source = source:sub(4)
	end

	source = self:parseHeader(source)

	-- add default define
	local slist = self.m_default
	table.insert(slist, source)
	-- Compiler generates "error X3000: syntax error: unexpected end of file"
	-- if input doesn't have empty line at EOF.
	if source:sub(-1) ~= "\n" then
		table.insert(slist, "\n")
	end

	source = table.concat(slist)

	local function file_loader(filename)
		local s = source_list[filename]
		if s == nil then
			error( filename .. " is not found")
		end
		return s
	end
	local r = lcpp.compile { code = source , define = self.m_define ,loader = file_loader }
	if r:find "\r" then
		r = r:gsub("\r\n", "\n")
		r = r:gsub("\r", "\n")
	end

	if self.m_raw then
		return self:genRaw(r)
	end
	if self.m_shaderType == 'c' then
		r = self:preprocessComputeShader(r)
	else
		r = self:preprocessShader(r)
	end
	return lcpp.compile { code = r, define = self.m_define, loader = file_loader }
end

-- uniforms

local uniform_type = {
	int1 = 0,
	vec4 = 2,
	mat3 = 3,
	mat4 = 4,
}

function Preprocessor:getUniforms(source)
	local list = {}
	if not self.m_lang.metal then
		-- uniform type ... ;
		for line in source:gmatch("[^;]+") do
			local what, typ, name, tail = line:match("^%s*(%a+)%s+(%w+)%s+([%w_]+)%s*(.*)")
			if what == "uniform" then
				if precision_keyword[typ] then
					typ, name, tail = name, tail:match("^([%w_]+)%s*(.*)")
				end
				if typ ~= "tmpvar" then
					if typ == "sampler" then
						typ = "int"
					end
					local num = tail:match("^%[%s*%d%s*%]")
					if num then
						typ = typ .. num
						num = tonumber(num)
					else
						num = 1
					end
					local typeid = uniform_type[typ]
					assert(typeid, "Invalid uniform type")
					table.insert(list, { name = name, type = typeid, num = num })
				end
			end
		end
	else
		local uniform_text = source:match("struct%s+xlatMtlShaderUniform%s*(%b{})")
		for line in uniform_text:gmatch("[^;]+") do
			local typ, name, tail = line:match("^%s*(%a+)%s+(%w+)%s*(.*)")
			local num = tail:match("^%[%s*%d%s*%]")
			if num then
				typ = typ .. num
				num = tonumber(num)
			else
				num = 1
			end
			local typeid = uniform_type[typ]
			assert(typeid, "Invalid uniform type")
			table.insert(list, { name = name, type = typeid, num = num })
		end
	end
	return list
end

function Preprocessor:writeUniforms(data, uniforms)
		table.insert(data, string.pack("<I2", #uniforms))
		for _, u in ipairs(uniforms) do
			table.insert(data, string.pack("<s1BBI2I2", u.name, u.type, u.num, 0, u.num))
		end
end

--- compile

local function trim_all_directives(source)
	local index = 1
	while source:sub(index, index) == "#" do
		index = source:find("\n", index, true) + 1
	end
	return source:sub(index)
end

local ARBEXT_NEED_REPLACE = {
	texture2DLod = "texture2DLod",
	texture2DGrad = "texture2DGrad",
	textureCubeLod = "textureCubeLod",
	textureCubeGrad = "textureCubeGrad",
	texture2DProjLod = "texture2DProjLod",
	texture2DProjGrad = "texture2DProjGrad",
	shadow2D = "shadow2D",
	shadow2DProj = "shadow2DProj",
}

local function optimize_glsl(source, opt)
	local glslopt = require "glslopt"
	local ok, r = glslopt.optimize(source, opt)
	assert(ok, r)

	r = trim_all_directives(r)
	r = r:gsub("(%w+)ARB", ARBEXT_NEED_REPLACE)
	r = r:gsub("(%w+)EXT", ARBEXT_NEED_REPLACE)
	r = r:gsub("(gl_FragDepth)EXT", "%1")

	return r
end

local s_ARB_shader_texture_lod = {
		"texture2DLod",
		"texture2DArrayLod", -- BK - interacts with ARB_texture_array.
		"texture2DProjLod",
		"texture2DGrad",
		"texture2DProjGrad",
		"texture3DLod",
		"texture3DProjLod",
		"texture3DGrad",
		"texture3DProjGrad",
		"textureCubeLod",
		"textureCubeGrad",
		"shadow2DLod",
		"shadow2DProjLod",
		-- "texture1DLod",
		-- "texture1DProjLod",
		-- "shadow1DLod",
		-- "shadow1DProjLod",
}

local s_EXT_shader_texture_lod = {
	"texture2DLod",
	"texture2DProjLod",
	"textureCubeLod",
	"texture2DGrad",
	"texture2DProjGrad",
	"textureCubeGrad",
}

local s_EXT_shadow_samplers = {
	"shadow2D",
	"shadow2DProj",
	"sampler2DShadow",
}

local s_OES_standard_derivatives = {
	"dFdx",
	"dFdy",
	"fwidth",
}

local s_OES_texture_3D = {
	"texture3D",
	"texture3DProj",
	"texture3DLod",
	"texture3DProjLod",
}

local s_EXT_gpu_shader4 = {
	"gl_VertexID",
	"gl_InstanceID",
}

local s_ARB_gpu_shader5 = {
	"bitfieldReverse",
	"floatBitsToInt",
	"floatBitsToUint",
	"intBitsToFloat",
	"uintBitsToFloat",
}

local s_ARB_shading_language_packing = {
	"packHalf2x16",
	"unpackHalf2x16",
}

local s_textureArray = {
	"texture2DArray",
	"texture2DArrayLod",
	"shadow2DArray",
}

local s_ARB_texture_multisample = {
	"sampler2DMS",
	"isampler2DMS",
	"usampler2DMS",
}

local s_texelFetch = {
	"texelFetch",
	"texelFetchOffset",
}

local s_130 = {
	"uint",
	"uint2",
	"uint3",
	"uint4",
	"isampler3D",
	"usampler3D",
}

local function findIdentifierMatch(source, ids)
	for _,v in ipairs(ids) do
		v = "[^%w_]" .. v .. "[^%w_]"
		if source:find(v) then
			return true
		end
	end
	return false
end

local function addGLExtensions(lang)
	local code = {}
	local function enable_extension(enable, what)
		if enable then
			table.insert(code, "extension " .. what .. " : enable")
		end
	end
	if lang.glsl and lang.glsl < 400 then
		local usesTextureLod = findIdentifierMatch(source, s_ARB_shader_texture_lod)
			or findIdentifierMatch(source, s_EXT_shader_texture_lod)
		local usesInstanceID = source:find("gl_InstanceID",1,true)
		local usesGpuShader4   = findIdentifierMatch(source, s_EXT_gpu_shader4)
		local usesGpuShader5   = findIdentifierMatch(source, s_ARB_gpu_shader5)
		local usesTexelFetch   = findIdentifierMatch(source, s_texelFetch)
		local usesTextureMS    = findIdentifierMatch(source, s_ARB_texture_multisample)
		local usesTextureArray = findIdentifierMatch(source, s_textureArray)
		local usesPacking      = findIdentifierMatch(source, s_ARB_shading_language_packing)
		if not lang.essl then
			local need130 = (120 == lang.glsl) and (usesTexelFetch or findIdentifierMatch(source, source))
			if lang.metal then
				table.insert(code, "#version 120")
			else
				table.insert(code, string.format("#version %d", need130 and 130 or lang.glsl))
			end
			enable_extension(usesInstanceID , "GL_ARB_draw_instanced")
			enable_extension(usesGpuShader4 , "GL_EXT_gpu_shader4")
			enable_extension(usesGpuShader5 , "GL_ARB_gpu_shader5")
			enable_extension(usesPacking, "GL_ARB_shading_language_packing")
			local ARB_shader_texture_lod = false
			local EXT_shader_texture_lod = false
			if usesTextureLod then
				if m_shaderType == 'f' then
					ARB_shader_texture_lod = true
				else
					EXT_shader_texture_lod = true
				end
			end
			enable_extension(ARB_shader_texture_lod, "GL_ARB_shader_texture_lod")
			enable_extension(EXT_shader_texture_lod, "GL_EXT_shader_texture_lod")
			enable_extension(usesTextureMS, "GL_ARB_texture_multisample")
			enable_extension(usesTextureArray, "GL_EXT_texture_array")
			if 130 > lang.glsl then
				table.insert(code, "#define ivec2 vec2\n#define ivec3 vec3\n#define ivec4 vec4")
			end
			if ARB_shader_texture_lod then
				table.insert(code, "#define texture2DProjLod  texture2DProjLodARB\n#define texture2DGrad     texture2DGradARB\n#define texture2DProjGrad texture2DProjGradARB\n#define textureCubeGrad   textureCubeGradARB")
			elseif EXT_shader_texture_lod then
				table.insert(code, "#define texture2DProjLod  texture2DProjLodEXT\n#define texture2DGrad     texture2DGradEXT\n#define texture2DProjGrad texture2DProjGradEXT\n#define textureCubeGrad   textureCubeGradEXT")
			end
			if need130 then
				table.insert(code, "#define bgfxShadow2D(_sampler, _coord)     vec4_splat(texture(_sampler, _coord))\n#define bgfxShadow2DProj(_sampler, _coord) vec4_splat(textureProj(_sampler, _coord))")
			else
				table.insert(code, "#define bgfxShadow2D     shadow2D\n#define bgfxShadow2DProj shadow2DProj\n")
			end
		else -- essl
			-- Pretend that all extensions are available.
			-- This will be stripped later.
			if usesTextureLod then
				table.insert(code,[[
#extension GL_EXT_shader_texture_lod : enable
#define texture2DLod      texture2DLodEXT
#define texture2DGrad     texture2DGradEXT
#define texture2DProjLod  texture2DProjLodEXT
#define texture2DProjGrad texture2DProjGradEXT
#define textureCubeLod    textureCubeLodEXT
#define textureCubeGrad   textureCubeGradEXT
]])
			end

			enable_extension(findIdentifierMatch(source, s_OES_standard_derivatives) , "GL_OES_standard_derivatives")
			enable_extension(findIdentifierMatch(source, s_OES_texture_3D) ,"GL_OES_texture_3D")
			if findIdentifierMatch(source, s_EXT_shadow_samplers) then
				table.insert(code,[[
#extension GL_EXT_shadow_samplers : enable
#define shadow2D shadow2DEXT
#define shadow2DProj shadow2DProjEXT
]])
			end
			enable_extension(usesGpuShader5, "GL_ARB_gpu_shader5")
			enable_extension(usesPacking, "GL_ARB_shading_language_packing")
			if findIdentifierMatch(source, "gl_FragDepth") then
				table.insert(code, "#extension GL_EXT_frag_depth : enable\n#define gl_FragDepth gl_FragDepthEXT")
			end
			enable_extension(usesTextureArray, "GL_EXT_texture_array")
			table.insert(code, "#define ivec2 vec2\n#define ivec3 vec3\n#define ivec4 vec4")
		end
	else
		table.insert(code, "#version " .. glsl)
	end
	return code
end

function Preprocessor:compile(source)
	local lang = self.m_lang
	if self.m_raw then
		source = string.pack("<I2s4B", 0, source, 0)
		-- todo: compile raw
		return source
	end

	local data = {
		BGFX_CHUNK_MAGIC[self.m_shaderType],
		string.pack("<I4", (self.m_shaderType == 'f' and self.m_inputHash or self.m_outputHash) or 0),
	}
	if self.m_shaderType == 'c' then
		if lang.glsl or lang.essl then
			if lang.essl then
				source = "#version 310 es\n" .. source
			else
				source = string.format("#version %d\n%s", lang.glsl or 430 , source)
			end
		elseif lang.spirv then
			error "Not support SPIRVSL"
		elseif lang.pssl then
			error "Not support PSSL"
		else
			error "Not support HLSL"
		end
	else	-- // Vertex/Fragment
		if lang.glsl or lang.essl or lang.metal then
			local code = addGLExtensions(lang)
			table.insert(code, source)
			source = table.concat(code, "\n")
			if lang.glsl <= 400 then
				-- optimize
				local opt = self.m_shaderType
				if lang.metal then
					opt = opt .. "M"
				elseif lang.essl then
					opt = opt .. lang.essl
				end
				table.insert(data, optimize_glsl(source, opt))
				local uniforms = self:getUniforms(source)
				self:writeUniforms(data, uniforms)
			end
		else
			error "Not support"
		end
	end
	table.insert(data, string.pack("<I2s4B", 0, source, 0))
	return table.concat(data)
end

function shaderc.compile(args)
	local preprocessor = Preprocessor.new(args.profile, args.define, args.raw)
	preprocessor:setDefaultDefines()
	preprocessor:definePlatform(args.platform)
	preprocessor:setDefine("M_PI","3.1415926535897932384626433832795")
	preprocessor:defineShaderType(args.shaderType)
	preprocessor:parseVaryingMap(args.varyingdef)
	local source = preprocessor:parseSource(args.source)	-- { source_code , { filename = includesource } }
	if args.preprocess then
		return source
	end
	return preprocessor:compile(source)
end

return shaderc
