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

#include "common.h"
#include "shlwapi.h"
#include "map"
#include <sys/stat.h>

extern "C" 
{
	int HgQueryDirstateDirectory(const TCHAR* hgroot, const TCHAR* abspath, char* relpathloc, char* outStatus);
	int HgQueryDirstateFile(const TCHAR* hgroot, const TCHAR* abspath, char* relpathloc, char* outStatus);
}

BOOL HgFindRoot(LPCWSTR path, string* root)
{
	wchar_t temp1[MAX_PATH];
	wchar_t temp2[MAX_PATH];

	LPWSTR dir = temp1;
	LPWSTR other = temp2;

	if (!GetFullPathName(path, MAX_PATH, dir, NULL))
		return FALSE;

	BOOL found = FALSE;
	while (dir)
	{
		other = PathCombine(other, dir, L".\\.hg\\store");
		if (found = PathIsDirectory(other))
			break;

		// search parent
		if (PathIsRoot(dir))
			dir = NULL;
		else
		{
			LPWSTR temp = PathCombine(other, dir, L"..");
			other = dir;
			dir = temp;
		}
	}

	if (found)
		*root = dir;
	return found;
}

HRESULT HgGetStatusDirectory(const string repo, LPCWSTR abspath, const string relpath, char* pStatus, char* pChildrenStatus)
{
	*pStatus = *STATUS_REPO;

	char relpathloc[MAX_PATH];
	if (!WideCharToMultiByte(CP_UTF8, 0, relpath.c_str(), (int)relpath.length() + 1, relpathloc, MAX_PATH, NULL, NULL))
		return HRESULT_FROM_WIN32(GetLastError());

	if (HgQueryDirstateDirectory(repo.c_str(), abspath, relpathloc, pChildrenStatus))
		return S_OK;

	return S_OK;
}

struct CacheKey
{
	string file;
	__time64_t mtime;

	CacheKey() : mtime(-1)
	{
		
	}

	CacheKey(const CacheKey& key)
		: file(key.file), mtime(key.mtime)
	{
	}

	CacheKey(const string& file)
		: file(file)
	{
		struct _stat stat;
		if (0 == _tstat(file.c_str(), &stat))
			mtime = stat.st_mtime;
		else
			mtime = 0;
	}

	bool check() const
	{
		struct _stat stat = { 0 };
		_tstat(file.c_str(), &stat);
		return mtime != -1 && mtime == stat.st_mtime;
	}
};

struct CacheEntry
{
	CacheKey repo;
	CacheKey ignores;
	CacheKey key;
	char status;

	CacheEntry() {}

	CacheEntry(const CacheEntry& entry)
		: repo(entry.repo), ignores(entry.ignores),
		key(entry.key), status(entry.status)
	{
	}

	CacheEntry(const string& repo, const string& abspath, const char status)
		: repo(repo + L"/.hg/dirstate"), ignores(repo + L"/.hgignore"),
		key(abspath), status(status)
	{
	}

	bool check() const
	{
		return repo.check() && ignores.check() && key.check();
	}
};
std::map<string, CacheEntry> _Cache;

HRESULT HgGetStatusFile(const string repo, LPCWSTR abspath, const string relpath, char* pStatus, char* pChildrenStatus)
{
	char relpathloc[MAX_PATH];
	if (!WideCharToMultiByte(CP_UTF8, 0, relpath.c_str(), (int)relpath.length() + 1, relpathloc, MAX_PATH, NULL, NULL))
		return HRESULT_FROM_WIN32(GetLastError());

	// first try dir state
	if (HgQueryDirstateFile(repo.c_str(), abspath, relpathloc, pStatus))
		return S_OK;

	// cache all dirstate misses
	std::map<string, CacheEntry>::const_iterator it = _Cache.find(abspath);
	if (it != _Cache.end())
	{
		const CacheEntry* entry = &it->second;
		if (entry->check())
		{
			*pStatus = entry->status;
			return S_OK;
		}
	}

	// ask hg directly
	string hg;
	LPCWSTR cmdexe;
	Hg(&hg, &cmdexe);

	wchar_t cmd[32768];
	LPCWSTR args[] = 
	{
		hg.c_str(),
		repo.c_str(),
		relpath.c_str(),
		NULL,
	};

	// create command line
	if (!FormatMessage(FORMAT_MESSAGE_FROM_STRING | FORMAT_MESSAGE_ARGUMENT_ARRAY, L"\"%1\" -R \"%2\" status -A \"%3\"", 0, 0, cmd, 32768, (va_list*)args))
		return HRESULT_FROM_WIN32(GetLastError());

	// create pipe with inheritable handles
	SECURITY_ATTRIBUTES sa;
	memset(&sa, 0, sizeof(sa));
	sa.nLength = sizeof(sa);
	sa.bInheritHandle = TRUE;

	HANDLE hread, hwrite;
	if (!CreatePipe(&hread, &hwrite, &sa, 32768))
		return HRESULT_FROM_WIN32(GetLastError());

	// run cmd and redirect stdout to our pipe
	STARTUPINFO si;
    PROCESS_INFORMATION pi;

	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);

	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdOutput = hwrite;

	if (!CreateProcess(cmdexe, cmd, NULL, NULL, TRUE, CREATE_NO_WINDOW, NULL, repo.c_str(), &si, &pi))
		return HRESULT_FROM_WIN32(GetLastError());

	// read from our pipe
	char buffer[32768];
	DWORD read = 0;
	while (read == 0)
		if (!ReadFile(hread, &buffer, 32768, &read, NULL))
			return HRESULT_FROM_WIN32(GetLastError());

	CloseHandle(hread);

	// get the first char from the output
	// ignoreing the BOM if it is there.
	int ix = 0;
	if (buffer[ix] == 0xEF) ix++;
	if (buffer[ix] == 0xBB) ix++;
	if (buffer[ix] == 0xBF) ix++;
	*pStatus = buffer[ix];

	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);

	string stemp = abspath;
	_Cache[stemp] = CacheEntry(repo, stemp, *pStatus);

	return S_OK;
}

string _foundHg;
string _foundCmd;
void Hg(string* hg, LPCWSTR* cmd)
{
	if (_foundHg.empty())
	{
		wchar_t temp[MAX_PATH];

		errno_t err = _wsearchenv_s(L"hg.bat", L"PATH", temp)
			&& _wsearchenv_s(L"hg.exe", L"PATH", temp)
			&& _wsearchenv_s(L"hg.cmd", L"PATH", temp);

		if (err == 0)
			_foundHg = temp;
	}

	*hg = _foundHg;
	*cmd = _foundCmd.empty() ? NULL : _foundCmd.c_str();
}

DWORD   lastTime;
string  lastFile;
char    lastStatus;
char    lastChildrenStatus;

HRESULT STDAPICALLTYPE HgGetStatus(LPCWSTR path, char* pStatus, char* pChildrenStatus)
{
	DWORD now = GetTickCount();
	if (now - lastTime < 1000 && path == lastFile)
	{
		*pStatus = lastStatus;
		*pChildrenStatus = lastChildrenStatus;
		lastTime = now;
		return S_OK;
	}

	*pStatus = *STATUS_UNKNOWN;
	*pChildrenStatus = 0;

	BOOL isDir = PathIsDirectory(path);

	string hgroot;
	if (!HgFindRoot(path, &hgroot))
	{
		if (isDir)
			*pStatus = *STATUS_NONREPO;
		return S_OK;
	}

	size_t offset = hgroot.length();
	if (path[offset] == '\\')
		offset++;
	
	string relpath = path + offset;

	HRESULT hr;
	if (isDir)
		hr = HgGetStatusDirectory(hgroot, path, relpath, pStatus, pChildrenStatus);
	else
		hr = HgGetStatusFile(hgroot, path, relpath, pStatus, pChildrenStatus);

	if (SUCCEEDED(hr))
	{
		lastTime = GetTickCount();
		lastFile = path;
		lastStatus = *pStatus;
		lastChildrenStatus = *pChildrenStatus;
	}

    return hr;
}
