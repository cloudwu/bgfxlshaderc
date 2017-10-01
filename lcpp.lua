----------------------------------------------------------------------------
--## lcpp - a C-PreProcessor for Lua 5.3
--
-- Copyright (C) 2012-2014 Michael Schmoock <michael@schmoock.net>
--
-- Modify Version for Lua 5.3 by 云风 <cloudwu@gmail.com>
--		Add hook for include from memory
--
--### Links
-- * GitHub page:  http://github.com/cloudwu/lcpp
----------------------------------------------------------------------------
--## USAGE
--[=[
local lcpp = require "lcpp"
local filelist = {}

filelist.a = "aaa"
filelist.b = "bbb"

local function loader(filename, _local)
  local code = filelist[filename]
  if code == nil then
    error ("file : " .. filename .. " not exist")
  end
  return code
end

local r = lcpp.compile {
   code = [[
#include <a>
#include "b"
ccc\z
]],
	loader = loader,
}
]=]

--## This CPPs BNF:
--	RULES:
--	CODE              := {LINE}
--	LINE              := {STUFF NEWML} STUFF  NEWL
--	STUFF             := DIRECTIVE | IGNORED_CONTENT
--	DIRECTIVE         := OPTSPACES CMD OPTSPACES DIRECTIVE_NAME WHITESPACES DIRECTIVE_CONTENT WHITESPACES NEWL
--
--	LEAVES:
--	NEWL              := "\n"
--	NEWL_ESC          := "\\n"
--	WHITESPACES       := "[ \t]+"
--	OPTSPACES         := "[ \t]*"
--	COMMENT           := "//(.-)$"
--	MLCOMMENT         := "/[*](.-)[*]/"
--	IGNORED_CONTENT   := "[^#].*"
--	CMD               := "#"
--	DIRECTIVE_NAME    := "include"|"define"|"undef"|"if"|"else"|"elif"|"else if"|"endif"|"ifdef"|"ifndef"|"pragma"
--	DIRECTIVE_CONTENT := ".*?"
--
--## License (MIT)
-- -----------------------------------------------------------------------------
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
--
-- MIT license: http://www.opensource.org/licenses/mit-license.php
-- -----------------------------------------------------------------------------
--
-- @module lcpp
local lcpp = {}

-- CONFIG
lcpp.ENV              = {}      -- static predefines (env-like)
lcpp.FAST             = false   -- perf. tweaks when enabled. con: breaks minor stuff like __LINE__ macros
lcpp.DEBUG            = false

-- PREDEFINES
local __FILE__        = "__FILE__"
local __LINE__        = "__LINE__"
local __DATE__        = "__DATE__"
local __TIME__        = "__TIME__"
local __LCPP_INDENT__ = "__LCPP_INDENT__"

-- BNF LEAVES
local ENDL            = "$"
local STARTL          = "^"
local NEWL            = "\n"
local NEWL_BYTE       = NEWL:byte(1)
local NEWL_ESC        = "\\"
local NEWML           = "\\\n"
local CMD             = "#"
local CMD_BYTE        = CMD:byte(1)
local COMMENT         = "^(.-)//.-$"
local MLCOMMENT       = "/[*].-[*]/"
local WHITESPACES     = "%s+"
local OPTSPACES       = "%s*"
local IDENTIFIER      = "[_%a][_%w]*"
local NOIDENTIFIER    = "[^%w_]+"
local FILENAME        = "[0-9a-zA-Z.%-_/\\]+"
local TEXT            = ".+"
local STRINGIFY       = "#"
local STRINGIFY_BYTE  = STRINGIFY:byte(1)
local STRING_LITERAL  = ".*"

-- BNF WORDS
local _INCLUDE        = "include"
local _DEFINE         = "define"
local _IFDEF          = "ifdef"
local _IFNDEF         = "ifndef"
local _ENDIF          = "endif"
local _UNDEF          = "undef"
local _IF             = "if"
local _ELSE           = "else"
local _ELIF           = "elif"
local _NOT            = "!"
local _ERROR          = "error"
local _WARNING		  = "warning"
local _PRAGMA         = "pragma"

-- BNF RULES
local INCLUDE         = STARTL.._INCLUDE..WHITESPACES.."[<]("..FILENAME..")[>]"..OPTSPACES..ENDL
local LOCAL_INCLUDE   = STARTL.._INCLUDE..WHITESPACES.."[\"]("..FILENAME..")[\"]"..OPTSPACES..ENDL
local DEFINE          = STARTL.._DEFINE
local IFDEF           = STARTL.._IFDEF..WHITESPACES.."("..IDENTIFIER..")"..OPTSPACES..ENDL
local IFNDEF          = STARTL.._IFNDEF..WHITESPACES.."("..IDENTIFIER..")"..OPTSPACES..ENDL
local ENDIF           = STARTL.._ENDIF..OPTSPACES..ENDL
local UNDEF           = STARTL.._UNDEF..WHITESPACES.."("..IDENTIFIER..")"..OPTSPACES..ENDL
local IF              = STARTL.._IF..WHITESPACES.."(.*)"..ENDL
local ELSE            = STARTL.._ELSE..OPTSPACES..ENDL
local ELIF            = STARTL.._ELIF..WHITESPACES.."(.*)"..ENDL
local ELSEIF          = STARTL.._ELSE..WHITESPACES.._IF..WHITESPACES.."(.*)"..ENDL
local ERROR           = STARTL.._ERROR..WHITESPACES.."("..TEXT..")"..OPTSPACES..ENDL
local WARNING         = STARTL.._WARNING..WHITESPACES.."("..TEXT..")"..OPTSPACES..ENDL
local ERROR_NOTEXT    = STARTL.._ERROR..OPTSPACES..ENDL	--> not required when we have POSIX regex
local PRAGMA          = STARTL.._PRAGMA

-- speedups
local TRUEMACRO = STARTL.."("..IDENTIFIER..")%s*$"
local REPLMACRO = STARTL.."("..IDENTIFIER..")"..WHITESPACES.."(.+)$"
local FUNCMACRO = STARTL.."("..IDENTIFIER..")%(([_%s%w,]*)%)%s*(.*)"

-- Declare local function
local lcpp_compile, lcpp_compileFile

-- ------------
-- LOCAL UTILS
-- ------------
lcpp.STATE = {lineno = 0} -- current state for debugging the last operation
local function error(msg) _G.print(debug.traceback()); _G.error(string.format("lcpp ERR [%04i] %s", lcpp.STATE.lineno, msg)) end
local function print(msg) _G.print(string.format("lcpp INF [%04i] %s", lcpp.STATE.lineno, msg)) end

-- splits a string using a pattern into a table of substrings
local function gsplit(str, pat)
	local function _split(str, pat)
		local t = {}  -- NOTE: use {n = 0} in Lua-5.0
		local fpat = "(.-)"..pat
		local last_end = 1
		local s, e, cap = str:find(fpat, 1)
		while s do
			if s ~= 1 or cap ~= "" then
				coroutine.yield(cap)
			end
			last_end = e + 1
			s, e, cap = str:find(fpat, last_end)
		end
		if last_end <= #str then
			cap = str:sub(last_end)
			coroutine.yield(cap)
		end
	end
	return coroutine.wrap(function() _split(str, pat) end)
end
local function split(str, pat)
	local t = {}
	for str in gsplit(str, pat) do table.insert(t, str) end
	return t
end

-- Checks whether a string starts with a given substring
-- offset is optional
local function strsw(str, pat, offset)
	if not str then return false end
	if not offset then offset = 0 end
	return string.sub(str, 1+offset, string.len(pat)+offset) == pat
end

-- Checks whether a string ends with a given substring
local function strew(str, pat)
	if not str then return false end
	return pat=='' or string.sub(str,-string.len(pat)) == pat
end

-- string trim12 from lua wiki
local function trim(str)
	local from = str:match"^%s*()"
	return from > #str and "" or str:match(".*%S", from)
end

-- returns the number of string occurrences
local function findn(input, what)
	local count = 0
	local offset = 0
	local _
	while true do
			_, offset = string.find(input, what, offset+1, true)
			if not offset then return count end
			count = count + 1
	end
end

-- C literal string concatenation
local function concatStringLiteral(input)
	-- screener does remove multiline definition, so just check ".*"%s*".*" pattern
	return input:gsub("\"("..STRING_LITERAL..")\""..OPTSPACES.."\"("..STRING_LITERAL..")\"", "\"%1%2\"")
end

-- c style boolean check (thus, 0 will be false)
local function CBoolean(value)
	return (not not value) and (value ~= 0)
end

-- eval with c style number parse (UL, LL, L)
local function CEval(expr)
	local ok, r = pcall(load, "return " .. parseCInteger(expr))
	if ok and r then
		return r()
	else
		error(r)
	end
end

-- a lightweight and flexible tokenizer
local function _tokenizer(str, setup)
		local defsetup = {
			-- EXAMPLE patterns have to be pretended with "^" for the tokenizer
			["identifier"] = '^[_%a][_%w]*',
			["number"] = '^[%+%-]?%d+[%.]?%d*[UL]*',
			["ignore"] = '^%s+',
			["string"] = true,
			["keywords"] = {
				-- ["NAME"] = '^pattern',
				-- ...
			},
		}
	if not setup then
		setup = defsetup
	end
	setup.identifier = setup.identifier or defsetup.identifier
	setup.number = setup.number or defsetup.number
	setup.ignore = setup.ignore or defsetup.ignore
	if nil == setup.string then setup.string = true end
	setup.keywords = setup.keywords or {}

	local strlen = #str
	local i = 1
	local i1, i2
	local keyword

	local function find(pat)
		i1, i2 = str:find(pat,i)
		return i1 ~= nil
	end

	local function cut()
		return str:sub(i, i2)
	end

	local findKeyword
	if setup.keywords_order then
		findKeyword = function ()
			for _, name in ipairs(setup.keywords_order) do
				assert(setup.keywords[name])
				local pat = setup.keywords[name]
				local result = find(pat)
				if result then
					keyword = name
					return true
				end
			end
		end
	else
		findKeyword = function ()
			for name, pat in pairs(setup.keywords) do
				local result = find(pat)
				if result then
					keyword = name
					return true
				end
			end
		end
	end

	while true do
		if i > strlen then return 'eof', nil, strlen, strlen end
		if findKeyword() then
			coroutine.yield(keyword, cut(), i1, i2)
		elseif find(setup.ignore) then
			coroutine.yield("ignore", cut(), i1, i2)
		elseif find(setup.number) then
			coroutine.yield('number', tonumber(cut()), i1, i2)
		elseif find(setup.identifier) then
			coroutine.yield('identifier', cut(), i1, i2)
		elseif setup.string and (find('^"[^"]*"') or find("^'[^']*'")) then
			-- strip the quotes
			coroutine.yield('string', cut():sub(2,-2), i1, i2)
		else -- any other unknown character
			i1 = i
			i2 = i
			coroutine.yield('unknown', cut(), i1, i2)
		end
		i = i2+1
	end
end
local function tokenizer(str, setup)
	return coroutine.wrap(function() _tokenizer(str, setup) end)
end


-- ------------
-- PARSER
-- ------------

local LCPP_TOKENIZE_COMMENT = {
	string = false,
	keywords = {
		MLCOMMENT = "^/%*.-%*/",
		SLCOMMENT = "^//.-\n",
		STRING_LITERAL = '^"[^"]*"',
	},
}
-- hint: LuaJIT ffi does not rely on us to remove the comments, but maybe other usecases
local function removeComments(input)
	local out = {}
	for k, v, start, end_ in tokenizer(input, LCPP_TOKENIZE_COMMENT) do
		if k == "MLCOMMENT" then
			local newlineCount = findn(input:sub(start, end_), "\n")
			local newlines = string.rep("\n", newlineCount + 1)
			table.insert(out, newlines)
		elseif k == "SLCOMMENT" then
			table.insert(out, "\n")
		else
			table.insert(out, input:sub(start, end_))
		end
	end
	return table.concat(out)
end

-- C style number parse (UL, LL, L) and (octet, hex, binary)
local LCPP_TOKENIZE_INTEGER = {
	string = false,
	keywords_order = {
		"STRING_LITERAL",
		"CHAR_LITERAL",
		"HEX_LITERAL",
		"BIN_LITERAL",
		"OCT_LITERAL",
		"FPNUM_LITERAL",
		"NUMBER_LITERAL",
	},
	keywords = {
		STRING_LITERAL = '^"[^"]*"',
		CHAR_LITERAL = "^L'.*'",
		HEX_LITERAL = '^[%+%-]?%s*0x[a-fA-F%d]+[UL]*',
		BIN_LITERAL = '^[%+%-]?%s*0b%d+[UL]*',
		OCT_LITERAL = '^[%+%-]?%s*0%d+[UL]*',
		FPNUM_LITERAL = '^[%+%-]?%s*%d+[%.]?%d*e[%+%-]%d*',
		NUMBER_LITERAL = '^[%+%-]?%s*%d+[%.]?%d*[UL]+',
	},
}
local function parseCInteger(input)
	-- print('parseCInteger:input:' .. input)
	local out = {}
	local unary
	for k, v, start, end_ in tokenizer(input, LCPP_TOKENIZE_INTEGER) do
	-- print('parseCInteger:' .. k .. "|" .. v)
		if k == "CHAR_LITERAL" then
			table.insert(out, tostring(string.byte(load("return \"" .. v:gsub("^L%'(.+)%'", "%1") .. "\"")())))
		elseif k == "HEX_LITERAL" then
			unary, v = v:match('([%+%-]?)0x([a-fA-F%d]+)[UL]*')
			local n = tonumber(v, 16)
			table.insert(out, unary..tostring(n))
		elseif k == "NUMBER_LITERAL" then
			v = v:match('([^UL]+)[UL]+')
			table.insert(out, v)
		elseif k == "BIN_LITERAL" then
			unary, v = v:match('([%+%-]?)0b([01]+)[UL]*')
			local n = tonumber(v, 2)
			table.insert(out, unary..tostring(n))
		elseif k == "OCT_LITERAL" then
			unary, v = v:match('([%+%-]?)(0%d+)[UL]*')
			local n = tonumber(v, 8)
			table.insert(out, unary..tostring(n))
		else
			table.insert(out, input:sub(start, end_))
		end
	end
	local str = table.concat(out)
	-- print('parseCInteger:result:'..str)
	return str
end

-- screener: revmoce comments, trim, ml concat...
-- it only splits to cpp input lines and removes comments. it does not tokenize.
local function screener(input)
	local function _screener(input)
		input = removeComments(input)

		local lines = {}
		local from
		local lineno = 1
		for l in gsplit(input, NEWL) do
			lines[lineno] = l
			if l:sub(-1) == "\\" then
				if from == nil then
					from = lineno
				end
				l = l:sub(1,-2)
			else
				if from then
					-- concat multi-line
					lines[from] = table.concat(lines, from, lineno)
					for i=from+1,lineno do
						lines[i] = ""
					end
					from = nil
				end
			end
			lineno = lineno + 1
		end

		-- trim and join blocks not starting with "#"
		local buffer = {}

		for _, line in ipairs(lines) do
			--print('newline:'..line)
			-- line = trim(line)
			if #line > 0 then
				if line:byte(1) == CMD_BYTE then
					line = line:gsub("#%s*(.*)", "#%1")	-- remove optinal whitespaces after "#". reduce triming later.
					if #buffer > 0 then
						coroutine.yield(table.concat(buffer, NEWL))
						buffer = {}
					end
					coroutine.yield(line)
				else
					if lcpp.FAST then
						table.insert(buffer, line)
					else
						coroutine.yield(line)
					end
				end
			elseif not lcpp.FAST then
				coroutine.yield(line)
			end
		end
		if #buffer > 0 then
			coroutine.yield(table.concat(buffer, NEWL))
		end
	end

	return coroutine.wrap(function() _screener(input) end)
end

-- apply currently known macros to input (and returns it)
local LCPP_TOKENIZE_APPLY_MACRO = {
	keywords = {
		DEFINED = "^defined%s*%(%s*"..IDENTIFIER.."%s*%)"	,
	},
}
local function apply(state, input)
	local already_expand = {}
	while true do
		local out = {}
		local functions = {}
		local expand

		for k, v, start, end_ in tokenizer(input, LCPP_TOKENIZE_APPLY_MACRO) do
			-- print('tokenize:'..tostring(k).."|"..tostring(v))
			if k == "identifier" then
				local repl = v
				local macro = state.defines[v]
				if macro then
					if type(macro)     == "boolean" then
						repl = ""
						expand = true
					elseif type(macro) == "string" then
						repl = macro
						already_expand[repl] = true
						expand = already_expand[v]
						already_expand[v] = true
					elseif type(macro) == "number" then
						repl = tostring(macro)
						already_expand[repl] = true
						expand = already_expand[v]
						already_expand[v] = true
					elseif type(macro) == "function" then
						local decl,cnt = input:sub(start):gsub("^[_%a][_%w]*%s*%b()", "%1")
						-- print('matching:'..input.."|"..decl.."|"..cnt)
						if cnt > 0 then
							repl = macro(decl)
							-- print("d&r:"..decl.."|"..repl)
							expand = true
							table.insert(out, repl)
							table.insert(out, input:sub(end_ + #decl))
							break
						else
							if input:sub(start):find("^[_%a][_%w]*%s*%(") then
								-- that is part of functional macro declaration.
								-- print(v ..': cannot replace:<'..input..'> read more line')
								return input,true
							else
								-- on macro name is also used as the symbol of some C declaration
								-- (e.g. /usr/include/spawn.h, /usr/include/sys/select.h on centos 6.4)
								-- no need to preprocess.
								print(v .. ': macro name but used as C declaration in:' .. input)
							end
						end
					end
				end
				table.insert(out, repl)
			elseif k == "DEFINED" then
				table.insert(out, input:sub(start, end_))
			else
				table.insert(out, input:sub(start, end_))
			end
		end
		input = table.concat(out)
		if not expand then
			break
		end
	end

	-- C liberal string concatenation, processing U,L,UL,LL
	return parseCInteger(concatStringLiteral(input)),false
end

-- processes an input line. called from lcpp doWork loop
local function processLine(state, line)
	if not line or #line == 0 then return line end
	local cmd = nil
	if line:byte(1) == CMD_BYTE then cmd = line:sub(2) end
	-- print("process:"..line)--.."|"..tostring(state:skip()))

	--[[ IF/THEN/ELSE STRUCTURAL BLOCKS ]]--
	if cmd then
		local ifdef   = cmd:match(IFDEF)
		local ifexp   = cmd:match(IF)
		local ifndef  = cmd:match(IFNDEF)
		local elif    = cmd:match(ELIF)
		local elseif_ = cmd:match(ELSEIF)
		local else_   = cmd:match(ELSE)
		local endif   = cmd:match(ENDIF)
		local struct  = ifdef or ifexp or ifndef or elif or elseif_ or else_ or endif

		if struct then
			local skip = state:skip()
			if ifdef   then state:openBlock(state:defined(ifdef))      end
			-- if skipped, it may have undefined expression. so not parse them
			if ifexp   then state:openBlock(skip and true or CBoolean(state:parseExpr(ifexp)))    end
			if ifndef  then state:openBlock(not state:defined(ifndef)) end
			if elif    then state:elseBlock((skip and skip < #state.stack) and true or CBoolean(state:parseExpr(elif)))     end
			if elseif_ then state:elseBlock((skip and skip < #state.stack) and true or CBoolean(state:parseExpr(elseif_)))  end
			if else_   then state:elseBlock(true)                      end
			if endif   then state:closeBlock()                         end
			return -- remove structural directives
		end
	end


	--[[ SKIPPING ]]--
	if state:skip() then
--		print('skip:' .. line)

		return
	end


	--[[ READ NEW DIRECTIVES ]]--
	if cmd then
		-- handle #undef ...
		local key = cmd:match(UNDEF)
		if type(key) == "string" then
			state:undefine(key)
			return
		end

		-- read "#define >FooBar...<" directives
		if cmd:match(DEFINE) then
			local define = trim(cmd:sub(DEFINE:len()+1))
			local macroname, replacement

			-- simple "true" defines
			macroname = define:match(TRUEMACRO)
			if macroname then
				state:define(macroname, true)
			else

			-- replace macro defines
			macroname, replacement = define:match(REPLMACRO)
			if macroname and replacement then
				state:define(macroname, replacement)
			else

			-- read functional macros
			macroname, replacement, source = state:parseFunction(define)
			if macroname and replacement then
				-- add original text for definition to check identify
				state:define(macroname, replacement, false, source)
			end

			end
			end

			return
		end

		-- handle #include ...
		local filename = cmd:match(INCLUDE)
		if filename then
			return state:includeFile(filename)
		end
		local filename = cmd:match(LOCAL_INCLUDE)
		if filename then
			return state:includeFile(filename, true)
		end

		-- ignore, because we dont have any pragma directives yet
		if cmd:match(PRAGMA) then return end

		-- handle #error
		local errMsg = cmd:match(ERROR)
		local errNoTxt = cmd:match(ERROR_NOTEXT)
		local warnMsg = cmd:match(WARNING)
		if errMsg then error(errMsg) end
		if errNoTxt then error("<ERROR MESSAGE NOT SET>") end
		if warnMsg then
			print(warnMsg)
			return
		end

		-- abort on unknown keywords
		error("unknown directive: "..line)
	end

	if state.incompleteLine then
		--print('merge with incompleteLine:'..state.incompleteLine)
		line = (state.incompleteLine .. line)
		state.incompleteLine = nil
	end


	--[[ APPLY MACROS ]]--
	-- print(line)
	local _line,more = state:apply(line);
	-- print('endprocess:'.._line)
	if more then
		state.incompleteLine = line
		return ""
	else
		return _line
	end

	return line
end

local function doWork(state)
	local function _doWork(state)
		if not state:defined(__FILE__) then state:define(__FILE__, "<USER_CHUNK>", true) end
		local oldIndent = state:getIndent()
		while true do
			local input = state:getLine()
			if not input then break end
			local output = processLine(state, input)
			if not lcpp.FAST and not output then output = "" end -- output empty skipped lines
			if lcpp.DEBUG then output = output.." -- "..input end -- input as comment when DEBUG
			if output then coroutine.yield(output) end
		end
		if (oldIndent ~= state:getIndent()) then error("indentation level must be balanced within a file. was:"..oldIndent.." is:"..state:getIndent()) end
	end
	return coroutine.wrap(function() _doWork(state) end)
end

local function includeFile(state, filename, _local)
	local result, result_state = lcpp_compileFile(filename, state.defines, state.macro_sources, _local, state.loadfunc)
	-- now, we take the define table of the sub file for further processing
	state.defines = result_state.defines
	-- and return the compiled result
	return result
end

-- sets a global define
local function define(state, key, value, override, macro_source)
	--print("define:"..key.." type:"..tostring(value).." value:"..tostring(pval))
-- todo:
--	if key ~= "__LINE__" then
--		_G.print(state.lineno, key, value)
--	end
	if value and not override then
		if type(value) == 'function' then
			assert(macro_source, "macro source should specify to check identity")
			local pval = state.macro_sources[key]
			if pval and (pval ~= macro_source) then error("already defined: "..key) end
			state.macro_sources[key] = macro_source
		else
			local pval = state.defines[key]
			if pval and (pval ~= value) then error("already defined: "..key) end
		end
	end
	state.defines[key] = state:prepareMacro(value)
end

-- parses CPP exressions
-- i.e.: #if !defined(_UNICODE) && !defined(UNICODE)
--
--BNF:
--  EXPR     -> (BRACKET_OPEN)(EXPR)(BRACKET_CLOSE)
--  EXPR     -> (EXPR)(OR)(EXPR)
--  EXPR     -> (EXPR)(AND)(EXPR)
--  EXPR     -> (NOT)(EXPR)
--  EXPR     -> (FUNCTION)
--  FUNCTION -> (IDENTIFIER)(BRACKET_OPEN)(ARGS)(BRACKET_CLOSE)
--  ARGS     -> ((IDENTIFIER)[(COMMA)(IDENTIFIER)])?
--LEAVES:
--  IGNORE -> " \t"
--  BRACKET_OPEN  -> "("
--  BRACKET_CLOSE -> ")"
--  OR -> "||"
--  AND -> "&&"
--  NOT -> "!"
--  IDENTIFIER -> "[0-9a-zA-Z_]"
--

local LCPP_TOKENIZE_MACRO = {
	string = true,
	keywords_order = {
		"CONCAT",
		"SPACE",
	},
	keywords = {
		CONCAT = "^%s*##%s*",
		SPACE = "^%s",
	},
}
local LCPP_TOKENIZE_MACRO_ARGS = {
	string = true,
	keywords_order = {
		"STRING_LITERAL",
		"PARENTHESE",
		"FUNCTIONAL",
		"ARGS",
		"SINGLE_CHARACTER_ARGS",
		"COMMA",
	},
	keywords = {
		PARENTHESE = "^%s*%b()",
		FUNCTIONAL = "^".. IDENTIFIER .. "%s*%b()",
		STRING_LITERAL = '^"[^"]*"',
		ARGS = "^[^,%s][^,]*[^,%s]",
		SINGLE_CHARACTER_ARGS = "^[^,%s]",
		COMMA = "^,",
	},
}
local LCPP_TOKENIZE_EXPR = {
	string = false,
	keywords_order = {
		"DEFINED",
		"FUNCTIONAL_MACRO",
		"BROPEN",
		"BRCLOSE",

		"TENARY_START",
		"TENARY_MIDDLE",
		-- binary operators
		"EQUAL",
		"NOT_EQUAL",
		"AND",
		"OR",
		"BAND",
		"BOR",
		"BXOR",
		"PLUS",
		"MINUS",
		"MULTIPLY",
		"DIV",
		"MOD",
		"LTE",
		"MTE",
		"LSHIFT",
		"RSHIFT",
		"LT",
		"MT",
		-- unary operator
		"NOT",
		"BNOT",
		-- literal
		"STRING_LITERAL",
		"CHAR_LITERAL",
		"HEX_LITERAL",
		"FPNUM_LITERAL",
		"NUMBER_LITERAL",
	},
	keywords = {
		DEFINED = '^defined',
		FUNCTIONAL_MACRO = '^' .. IDENTIFIER .. "%s*%b()",
		BROPEN = '^[(]',
		BRCLOSE = '^[)]',

		TENARY_START = '^%?',
		TENARY_MIDDLE = '^%:',

		EQUAL = '^==',
		NOT_EQUAL = '^!=',
		AND = '^&&',
		OR = '^||',
		BAND = '^&',
		BOR = '^|',
		BXOR = '^%^',
		PLUS = '^%+',
		MINUS = '^%-',
		MULTIPLY = '^%*',
		DIV = '^%/',
		MOD = '^%%',
		LTE = '^<=',
		MTE = '^>=',
		LSHIFT = '^<<',
		RSHIFT = '^>>',
		LT = '^<',
		MT = '^>',

		NOT = '^!',
		BNOT = '^~',

		STRING_LITERAL = '^L?"[^"]*"',
		CHAR_LITERAL = "^L?'.*'",
		HEX_LITERAL = '^[%+%-]?0?x[a-fA-F%d]+[UL]*',
		FPNUM_LITERAL = '^[%+%-]?%d+[%.]?%d*e[%+%-]%d*',
		NUMBER_LITERAL = '^[%+%-]?0?b?%d+[%.]?%d*[UL]*',
	},
}

local function parseDefined(state, input)
	local result = false
	local bropen = false
	local brclose = false
	local ident = nil

	for key, value in input do
		if key == "BROPEN" then
			bropen = true
		end
		if key == "identifier" then
			 ident = value
			 if not bropen then break end
		end
		if key == "BRCLOSE" and ident then
			brclose = true
			break
		end
	end

	-- wiht and w/o brackets allowed
	if ident and ((bropen and brclose) or (not bropen and not brclose)) then
		return state:defined(ident)
	end

	error("expression parse error: defined(ident)")
end


--[[
order : smaller is higher priority
1	()   []   ->   .
2	 !   ~   -   +   *   &   sizeof   type cast   ++   --
3	*   /   %
4	+   -
5	<<   >>
6	<   <=   >   >=
7	==   !=
8	&
9	^
10	|
11	&&
12	||
13	 ?:   =   +=   -=   *=   /=   %=   &=   |=   ^=   <<=   >>=
14	,
]]
local combination_order = function (op, unary)
	if unary then
		if op == '-' or op == '!' or op == '~' then
			return 2
		else
			assert(false, 'unsupported unary operator:' .. op)
		end
	else
		if op == '*' or op == '/' or op == '%' then
			return 3
		elseif op == '+' or op == '-' then
			return 4
		elseif op == '>>' or op == '<<' then
			return 5
		elseif op == '<' or op == '>' or op == '<=' or op == '>=' then
			return 6
		elseif op == '==' or op == '!=' then
			return 7
		elseif op == '&' then
			return 8
		elseif op == '^' then
			return 9
		elseif op == '|' then
			return 10
		elseif op == '&&' then
			return 11
		elseif op == '||' then
			return 12
		elseif op == '?' or op == ':' then
			return 13
		else
			assert(false, 'unsupported operator:' .. op)
		end
	end
end

local evaluate
evaluate = function (node)
	if not node.op then -- leaf node or leaf node with unary operators
		local v = node.v
		if node.uops then
			for _, uop in ipairs(node.uops) do
			 	-- print('apply uop:'..uop.."|"..tostring(v))
				if uop == '-' then
					v = -v
				elseif uop == '!' then
					v = (not v)
				elseif uop == '~' then
					v = ~v
				else
					assert(false, 'invalid uop:' .. tostring(uop))
				end
			end
		end
		-- print('after apply:'..tostring(v))
		return v
	end
	-- print(node.op..':'..tostring(node.l.v or node.l.op).."("..type(node.l.v)..")|"..tostring(node.r.v or node.r.op).."("..type(node.r.v)..")")
	if node.op == '+' then -- binary operators
		return (evaluate(node.l) + evaluate(node.r))
	elseif node.op == '-' then
		return (evaluate(node.l) - evaluate(node.r))
	elseif node.op == '*' then
		return (evaluate(node.l) * evaluate(node.r))
	elseif node.op == '/' then
		return (evaluate(node.l) / evaluate(node.r))
	elseif node.op == '%' then
		return (evaluate(node.l) % evaluate(node.r))
	elseif node.op == '==' then
		return (evaluate(node.l) == evaluate(node.r))
	elseif node.op == '!=' then
		return (evaluate(node.l) ~= evaluate(node.r))
	elseif node.op == '<<' then
		return evaluate(node.l) << evaluate(node.r)
	elseif node.op == '>>' then
		return evaluate(node.l) >> evaluate(node.r)
	elseif node.op == '&&' then
		return (CBoolean(evaluate(node.l)) and CBoolean(evaluate(node.r)))
	elseif node.op == '||' then
		return (CBoolean(evaluate(node.l)) or CBoolean(evaluate(node.r)))
	elseif node.op == '&' then
		return evaluate(node.l) & evaluate(node.r)
	elseif node.op == '|' then
		return evaluate(node.l) | evaluate(node.r)
	elseif node.op == '^' then
		return evaluate(node.l) ~ evaluate(node.r)
	elseif node.op == '<=' then
		return (evaluate(node.l) <= evaluate(node.r))
	elseif node.op == '>=' then
		return (evaluate(node.l) >= evaluate(node.r))
	elseif node.op == '<' then
		return (evaluate(node.l) < evaluate(node.r))
	elseif node.op == '>' then
		return (evaluate(node.l) > evaluate(node.r))
	else
		assert(false, 'invalid op:' .. tostring(node.op))
	end
end

local function setValue(node, v)
	-- print('setValue:' .. tostring(v).."|"..tostring(node.uops))-- .. "\t" .. debug.traceback())
	if not node.op then
		assert(not node.v, debug.traceback())
		node.v = v
	else
		assert(node.l and (not node.r))
		node.r = {v = v, uops = node.uops}
	end
end

local function setUnaryOp(node, uop)
	-- print('setUnaryOp:' .. tostring(uop))-- .. "\t" .. debug.traceback())
	if not node.uops then node.uops = {} end
	table.insert(node.uops, 1, uop)
end

local function parseExpr(state, input)
	local node = {}
	local root = node
	-- first call gets string input. rest uses tokenizer
	if type(input) == "string" then
		-- print('parse:' .. input)
		input = tokenizer(input, LCPP_TOKENIZE_EXPR)
	end

	for type, value in input do
		-- print("type:"..type.." value:"..value)
		-- unary operator
		if type == "NOT" or
			type == "BNOT" then
			setUnaryOp(node, value)
		end
		if type == "BROPEN" then
			setValue(node, state:parseExpr(input))
		end
		if type == "BRCLOSE" then
			--print('BRCLOSE:' .. tostring(result))
			break
		end
		if type == "STRING_LITERAL" then
			setValue(node, value:sub(value[1] == 'L' and 3 or 2,-2))
		end
		if type == "NUMBER_LITERAL" or type == "HEX_LITERAL" or type == "FPNUM_LITERAL" or type == "CHAR_LITERAL" then
			setValue(node, tonumber(parseCInteger(value)))
		end
		-- tenary operator
		-- tenary has lowest priority, so any other operation can be calculate now.
		if type == "TENARY_START" then
			local l = state:parseExpr(input)
			local r = state:parseExpr(input)
			if evaluate(root) then
				return l
			else
				return r
			end
		end
		if type == "TENARY_MIDDLE" then
			break
		end
		-- binary operator
		if type == "EQUAL" or
			type == "NOT_EQUAL" or
			type == "AND" or
			type == "OR" or
			type == "BAND" or
			type == "BOR" or
			type == "BXOR" or
			type == "PLUS" or
			type == "MINUS" or
			type == "MULTIPLY" or
			type == "DIV" or
			type == "MOD" or
			type == "LTE" or
			type == "MTE" or
			type == "LSHIFT" or
			type == "RSHIFT" or
			type == "LT" or
			type == "MT" then
			if node.op then
				if not node.r then -- during parse right operand : uop1 uop2 ... uopN operand1 op1 uop(N+1) uop(N+2) ... [uop(N+K)]
					assert(type == "MINUS",  "error: operators come consequently: " .. tostring(node.op) .. " and " .. tostring(value))
					-- unary operater after binary operator
					setUnaryOp(node, value)
				else -- uop1 uop2 ... uopN operand1 op1 uop(N+1) uop(N+2) ... uop(N+M) operand2 [op2]
					-- print("operator processing:" .. tostring(node.op) .. "|" .. value .. "|" .. tostring(node.l) .. "|" .. tostring(node.r))
					local tmp = node
					while tmp do
						-- print('compare ' .. value..' and ' .. tmp.op)
						if combination_order(tmp.op) > combination_order(value) then
							-- print(value..' is stronger than ' .. tmp.op)
							break
						end
						tmp = tmp.parent
					end
					if tmp then
						node = {
							op = value,
							l = tmp.r,
							parent = tmp
						}
						tmp.r.parent = node
						tmp.r = node
					else
						node = {
							op = value,
							l = root,
						}
						root.parent = node
						root = node
					end
				end
			elseif node.v ~= nil then -- uop1 uop2 ... uopN operand1 [op]
				local devided
				if node.uops then
					for _, uop in ipairs(node.uops) do
						if combination_order(uop, true) > combination_order(value) then
							-- there is a binary operator which has stronger than any of the unary
							devided = uop
						end
					end
				end
				if devided then
					assert(false, "TODO: can we do something about this case??:"..value.." is stronger than "..devided)
				else
					node.l = { v = node.v, uops = node.uops }
					node.v = nil
					node.uops = nil
					node.op = value
				end
			else -- unary operator : uop1 uop2 ... [uopN]
				assert(type == "MINUS", "error: invalid unary operator:" .. value)
				setUnaryOp(node, value)
			end
		end
		if type == "DEFINED" then
			setValue(node, parseDefined(state, input))
		elseif type == "identifier" or type == "FUNCTIONAL_MACRO" then
			-- print('ident:' .. value)
			local eval = state:apply(value)
			-- print('apply result ' .. eval .. "|" .. tostring(unprocessed))
			if eval ~= value then
				eval = state:parseExpr(eval)
				-- print('re-evaluate expr ' .. tostring(eval))
				setValue(node, eval)
			else
				-- undefined macro symbol is always treated as 0.
				-- http://gcc.gnu.org/onlinedocs/cpp/If.html#If
				setValue(node, 0)
			end
		end
	end

	local r = evaluate(root)
	-- print('evaluate:' .. tostring(r))
	return r
end

-- apply string ops "##"
local function prepareMacro(state, input)
	if type(input) ~= "string" then return input end
	repeat
		local out = {}
		local concat
		for k, v, start, end_ in tokenizer(input, LCPP_TOKENIZE_MACRO) do
			if k == "CONCAT" then
				-- remove concat op "##"
				concat = true
			else
				table.insert(out, input:sub(start, end_))
			end
		end
		input = table.concat(out)
	until not concat
	return input
end

-- macro args replacement function slower but more torelant for pathological case
local function replaceArgs(argsstr, repl)
	local args = {}
	argsstr = argsstr:sub(2,-2)
	-- print('argsstr:'..argsstr)
	local comma
	for k, v, start, end_ in tokenizer(argsstr, LCPP_TOKENIZE_MACRO_ARGS) do
		-- print("replaceArgs:" .. k .. "|" .. v)
		if k == "ARGS" or k == "PARENTHESE" or k == "STRING_LITERAL" or
			k == "FUNCTIONAL" or k == "SINGLE_CHARACTER_ARGS" then
			table.insert(args, v)
			comma = false
		elseif k == "COMMA" then
			if comma then
				-- continued comma means empty parameter
				table.insert(args, "")
			end
			comma = true
		end
	end
	local v = repl:gsub("%$(%d+)", function (m) return args[tonumber(m)] or "" end)
	-- print("replaceArgs:" .. repl .. "|" .. tostring(#args) .. "|" .. v)
	return v
end

-- i.e.: "MAX(x, y) (((x) > (y)) ? (x) : (y))"
local function parseFunction(state, input)
	if not input then return end
	local concat
	local name, argsstr, repl = input:match(FUNCMACRO)
	if not name or not argsstr or not repl then return end

	-- rename args to $1,$2... for later gsub
	local noargs = 0
	for argname in argsstr:gmatch(IDENTIFIER) do
		noargs = noargs + 1
		-- avoid matching substring of another identifier (eg. attrib matches __attribute__ and replace it)
		repl = repl:gsub("(#*)(%s*)("..argname..")([_%w]?)", function (s1, s2, s3, s4)
			if #s4 <= 0 then
				return (#s1 == 1) and ("\"$"..noargs.."\"") or (s1..s2.."$"..noargs)
			else
				return s1..s2..s3..s4
			end
		end)
	end
	-- remove concat (after replace matching argument name to $1, $2, ...)
	repl = repl:gsub("%s*##%s*", "")

	-- build macro funcion
	local func = function(input)
		return input:gsub(name.."%s*(%b())", function (match)
			return replaceArgs(match, repl)
		end)
	end

	return name, func, repl
end


-- ------------
-- LCPP INTERFACE
-- ------------

local function default_loader(filename, _local)
	local file = io.open(filename, 'r')
	if not file then error("file not found: "..filename) end
	local code = file:read('*a')
	file:close()
	return code
end

--- initialies a lcpp state. not needed manually. handy for testing
local function lcpp_init(input, predefines, macro_sources, loadfunc)
	-- create sate var
	local state     = {}              -- init the state object
	state.defines   = {}              -- the table of known defines and replacements
	state.screener  = screener(input)
	state.lineno    = 0               -- the current line number
	state.stack     = {}              -- stores wether the current stack level is to be included
	state.once      = {}              -- stack level was once true (first if that evals to true)
	state.macro_sources = macro_sources or {} -- original replacement text for functional macro
	state.loadfunc = loadfunc or default_loader

	-- funcs
	state.define = define
	state.undefine = function(state, key)
		state:define(key, nil)
		state.macro_sources[key] = nil
	end
	state.defined = function(state, key)
		return state.defines[key] ~= nil
	end
	state.apply = apply
	state.includeFile = includeFile
	state.doWork = doWork
	state.getIndent = function(state)
		return #state.stack
	end
	state.openBlock = function(state, bool)
		state.stack[#state.stack+1] = bool
		state.once [#state.once+1]  = bool
		state:define(__LCPP_INDENT__, state:getIndent(), true)
	end
	state.elseBlock = function(state, bool)
		if state.once[#state.once] then
			state.stack[#state.stack] = false
		else
			state.stack[#state.stack] = bool
			if bool then state.once[#state.once] = true end
		end
	end
	state.closeBlock = function(state)
		state.stack[#state.stack] = nil
		state.once [#state.once]  = nil
		state:define(__LCPP_INDENT__, state:getIndent(), true)
		if state:getIndent() < 0 then error("Unopened block detected. Indentaion problem.") end
	end
	state.skip = function(state)
		for i = 1, #state.stack do
			if not state.stack[i] then return i end
		end
		return false
	end
	state.getLine = function(state)
		state.lineno = state.lineno + 1
		state:define(__LINE__, state.lineno, true)
		return state.screener()
	end
	state.prepareMacro = prepareMacro
	state.parseExpr = parseExpr
	state.parseFunction = parseFunction

	-- predefines
	state:define(__DATE__, os.date("%B %d %Y"), true)
	state:define(__TIME__, os.date("%H:%M:%S"), true)
	state:define(__LINE__, state.lineno, true)
	state:define(__LCPP_INDENT__, state:getIndent(), true)
	for k,v in pairs(lcpp.ENV) do	state:define(k, v, true) end	-- static ones
	if predefines then
		for k,v in pairs(predefines) do	state:define(k, v, true) end
	end

	lcpp.STATE = state -- activate static state debugging

	return state
end

--- the preprocessors main function.
-- returns the preprocessed output as a string.
-- @param code data as string
-- @param predefines OPTIONAL a table of predefined variables
-- @usage lcpp.compile("#define bar 0x1337\nstatic const int foo = bar;")
-- @usage lcpp.compile("#define bar 0x1337\nstatic const int foo = bar;", {["bar"] = "0x1338"})
function lcpp_compile(code, predefines, macro_sources, loadfunc)
	local state = lcpp_init(code, predefines, macro_sources, loadfunc)
	local buf = {}
	for output in state:doWork() do
		table.insert(buf, output)
	end
	local output = table.concat(buf, NEWL)
	if lcpp.DEBUG then print(output) end
	return output, state
end

--- preprocesses a file
-- @param filename the file to read
-- @param predefines OPTIONAL a table of predefined variables
-- @usage out, state = lcpp.compileFile("../odbg/plugin.h", {["MAX_PAH"]=260, ["UNICODE"]=true})
function lcpp_compileFile(filename, predefines, macro_sources, _local, loadfunc)
	if not filename then error("processFile() arg1 has to be a string") end
	local code = loadfunc(filename, _local)
	predefines[__FILE__] = filename
	return lcpp_compile(code, predefines, macro_sources, loadfunc)
end

function lcpp.compile(args)
	return lcpp_compile(assert(args.code), args.define, nil, args.loader)
end

return lcpp
