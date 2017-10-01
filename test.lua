local shaderc = require "shaderc"

local args = {}

local function loadtext(filename)
	local f = io.open(filename,"rb")
	if not f then
		error (filename .. " not found")
	end
	local text = f:read "a"
	f:close()
	return text
end

local function loader(cache, filename)
	local name = filename:match "[^/]+$"
	local text = loadtext("shader/" .. name)
	cache[text] = text
	return text
end

args.varyingdef = loadtext "varying.def.sc"
args.source = setmetatable({ loadtext "fs_mesh.sc" } , { __index = loader })
args.shaderType = 'f'
args.profile = 150
args.platform = "windows"
--args.preprocess = true

local r = shaderc.compile(args)
-- print(r)


