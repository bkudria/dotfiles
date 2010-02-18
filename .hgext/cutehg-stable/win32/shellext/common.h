// CuteHg - Qt4 Dialog Extension of Mercurial
// Copyright (C) 2009 Stefan Rusek 
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#pragma once

#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include <tchar.h>

#ifdef USE_REAL_ATL
#include <atlbase.h>
#include <atlcom.h>
#else
#include "fakeatl.h"
#endif

#include "resource.h"
#include "cutehg_h.h"
#include <vector>
#include <string>

typedef std::wstring string;

#define STATUS_MOD			"M"
#define STATUS_CLEAN		"C"
#define STATUS_DEL			"R"
#define STATUS_ADD			"A"
#define STATUS_IGNORE 	    "I"
#define STATUS_UNTRACKED	"?"
#define STATUS_UNKNOWN		"_"
#define STATUS_HG_TRACKED	STATUS_MOD STATUS_CLEAN STATUS_DEL STATUS_ADD STATUS_IGNORE
#define STATUS_HG_FILE		STATUS_HG_TRACKED STATUS_UNTRACKED
#define STATUS_FILE			STATUS_HG_FILE STATUS_UNKNOWN

#define STATUS_REPO			"D"
#define STATUS_NONREPO		"N"
#define STATUS_DIR			STATUS_REPO STATUS_NONREPO

#define STATUS_ALL			STATUS_FILE STATUS_DIR
#define STATUS_HG_ALL		STATUS_HG_FILE STATUS_REPO

#define RETURN_FAILED(x) \
	{ HRESULT __hr = x; if (FAILED(__hr)) return __hr;}
#define RETURN_BAD_WIN32(x) \
	if (x) return HRESULT_FROM_WIN32(GetLastError())

HRESULT STDAPICALLTYPE HgGetStatus(LPCWSTR path, char* pStatus, char* pChildrenStatus);
BOOL HgFindRoot(LPCWSTR path, string* root);
void Hg(string* hg, LPCWSTR* cmd);

class CModule : public CAtlDllModuleT<CModule>
{
public:
	DECLARE_LIBID(LIBID_CuteHgLib);
	DECLARE_REGISTRY_APPID_RESOURCEID(IDR_LIBRARY, "{E586C961-E79B-479d-A08B-43557742195C}")
};

extern HINSTANCE _hInstance;
extern CModule _AtlModule;