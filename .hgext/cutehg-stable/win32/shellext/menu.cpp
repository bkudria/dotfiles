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
#include <objidl.h>
#include <gdiplus.h>

struct MenuCommand;
typedef HRESULT (CALLBACK *LPACTION)(const string&, const std::vector<string>*, const MenuCommand*);
struct MenuCommand
{
	LPCSTR   showstatus;
	LPCSTR   enablestatus;
	LPCWSTR  verb;
	LPCWSTR  text;
	LPCWSTR  help;
	LPCWSTR  cmdline;
	LPACTION action;
	UINT     icon;
	BOOL     showConsole;

	// calculated
	BOOL     enabled;
};

class ATL_NO_VTABLE CMenu :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CMenu, &CLSID_CuteHgMenu>, 
	public IShellExtInit, 
	public IContextMenu3,
	public ITest
{
public:
	DECLARE_CLASSFACTORY();
	DECLARE_NOT_AGGREGATABLE(CMenu);
	DECLARE_REGISTRY_RESOURCEID(IDR_MENU);

	BEGIN_COM_MAP(CMenu)
	   COM_INTERFACE_ENTRY(IShellExtInit)
	   COM_INTERFACE_ENTRY(IContextMenu)
	   COM_INTERFACE_ENTRY(IContextMenu2)
	   COM_INTERFACE_ENTRY(IContextMenu3)
	   COM_INTERFACE_ENTRY(ITest)
	END_COM_MAP()

	// IShellExtInit
	virtual HRESULT STDMETHODCALLTYPE Initialize(PCIDLIST_ABSOLUTE pidlFolder, IDataObject *pdtobj, HKEY hkeyProgID);

	// IContextMenu
    virtual HRESULT STDMETHODCALLTYPE QueryContextMenu(HMENU hmenu, UINT indexMenu, UINT idCmdFirst, UINT idCmdLast, UINT uFlags);
    virtual HRESULT STDMETHODCALLTYPE InvokeCommand(CMINVOKECOMMANDINFO *pici);
    virtual HRESULT STDMETHODCALLTYPE GetCommandString(UINT_PTR idCmd, UINT uType, UINT *pReserved, LPSTR pszName, UINT cchMax);

	// IContextMenu2
	virtual HRESULT STDMETHODCALLTYPE HandleMenuMsg(UINT uMsg, WPARAM wParam, LPARAM lParam);

	// IContextMenu3
	virtual HRESULT STDMETHODCALLTYPE HandleMenuMsg2(UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT *plResult);

	// ITest
	virtual HRESULT STDMETHODCALLTYPE Test(int a, LPCWSTR pwszMessage, int* pa, LPWSTR* pOut);

private:
	std::vector<string> m_Paths;
	std::vector<MenuCommand> m_Commands;
};
OBJECT_ENTRY_AUTO(CLSID_CuteHgMenu, CMenu);


HRESULT CALLBACK TestAction(const string&, const std::vector<string>*, const MenuCommand*);
HRESULT CALLBACK StandardAction(const string&, const std::vector<string>*, const MenuCommand*);
HRESULT CALLBACK StandardCuteAction(const string&, const std::vector<string>*, const MenuCommand*);
HRESULT CALLBACK StandardCuteNoFilesAction(const string&, const std::vector<string>*, const MenuCommand*);

static const MenuCommand _Commands[] = 
{
#ifdef DEBUG
	{STATUS_ALL, STATUS_ALL, L"cutehg_test", L"Test Integration", NULL, NULL, TestAction, IDB_CONFIGURE},
	{STATUS_ALL, STATUS_ALL, L"", L""},
#endif

	{STATUS_HG_ALL, STATUS_ALL, L"cutehistory", L"Browse History", NULL, NULL, StandardCuteNoFilesAction, IDB_EDIT_FIND},
	{STATUS_HG_ALL, STATUS_ALL, L"cutestatus", L"Show Status", NULL, NULL, StandardCuteNoFilesAction, IDB_SVN_STATUS},

	{STATUS_HG_ALL, STATUS_ALL, L"", L""},
	{STATUS_NONREPO, STATUS_NONREPO, L"init", L"Create Repository Here", NULL, NULL, StandardCuteAction, IDB_VCS_ADD},
	{STATUS_HG_TRACKED STATUS_REPO, STATUS_MOD STATUS_REPO, L"vdiff", L"Show Pending Changes", L"Show differences between revisions for the specified files", NULL, StandardCuteAction, IDB_TEXT_X_GENERIC, TRUE},
	{STATUS_HG_TRACKED STATUS_REPO, STATUS_MOD STATUS_REPO, L"commit", L"Commit Pending Changes", NULL, NULL, StandardCuteAction, IDB_IM_STATUS_MESSAGE_EDIT, TRUE},
	{STATUS_HG_TRACKED STATUS_REPO, STATUS_MOD STATUS_REPO, L"revert", L"Revert Pending Changes", NULL, NULL, StandardCuteAction, IDB_EDIT_UNDO, TRUE},
	{STATUS_HG_ALL, STATUS_ALL, L"cutepush", L"Push Changesets", NULL, NULL, StandardCuteNoFilesAction, IDB_VCS_COMMIT},
	{STATUS_HG_ALL, STATUS_ALL, L"cutepull", L"Pull Changesets", NULL, NULL, StandardCuteNoFilesAction, IDB_VCS_UPDATE},

	{STATUS_HG_ALL, STATUS_ALL, L"", L""},
	{STATUS_HG_ALL, STATUS_ALL, L"cuteupdate", L"Update to Revision", NULL, NULL, StandardCuteNoFilesAction, IDB_EDIT_UNDO},
	{STATUS_HG_ALL, "", L"cutebackout", L"Backup Previous Changes", NULL, NULL, StandardCuteNoFilesAction, IDB_DOCUMENT_REVERT},
	{STATUS_HG_TRACKED, STATUS_ALL, L"cuteannotate", L"Annotate Previous Changes", L"List changes in files, showing the revision id responsible for each line", NULL, StandardCuteAction, 0},
	{STATUS_HG_TRACKED, "", L"cutediff", L"Show Previous Changes", NULL, NULL, StandardCuteAction, IDB_VCS_DIFF},

	{STATUS_HG_ALL, STATUS_ALL, L"", L""},
	{STATUS_UNTRACKED, STATUS_UNTRACKED, L"add", L"Add to Mercurial", NULL, NULL, StandardCuteAction, IDB_SVN_ADD, TRUE},
	{STATUS_HG_TRACKED STATUS_REPO, STATUS_ALL, L"remove", L"Remove from Mercurial", NULL, NULL, StandardCuteAction, IDB_SVN_REMOVE, TRUE},

	{STATUS_HG_ALL, STATUS_ALL, L"", L""},
	{STATUS_HG_ALL, STATUS_ALL, L"serve -p 80", L"Run Server (port 80)", NULL, NULL, StandardCuteNoFilesAction, IDB_NETWORK_CONNECT, TRUE},
	{STATUS_HG_ALL, STATUS_ALL, L"serve", L"Run Server (port 8000)", NULL, NULL, StandardCuteNoFilesAction, IDB_NETWORK_CONNECT, TRUE},

	{0},
};

BOOL ChgLoadBitmap(UINT resid, Gdiplus::Bitmap** ppbitmap)
{
	HMODULE hmod = _hInstance;

	HRSRC hrsrc;
	if (!(hrsrc = FindResource(hmod, (LPCWSTR)resid, L"PNG")))
		return FALSE;

	DWORD bytes = SizeofResource(hmod, hrsrc);
	if (!bytes)
		return FALSE;

	LPVOID pRawData = LockResource(LoadResource(hmod, hrsrc));
	if (!pRawData)
		return FALSE;

	BOOL rval = FALSE;
	HGLOBAL hGlobTemp = GlobalAlloc(GMEM_MOVEABLE, bytes);
	if (hGlobTemp)
	{
		LPVOID pData = GlobalLock(hGlobTemp);
		if (pData)
		{
			memcpy(pData, pRawData, bytes);
			GlobalUnlock(pData);

			IStream* pStream = NULL;
			if (SUCCEEDED(CreateStreamOnHGlobal(hGlobTemp, FALSE, &pStream)))
			{
				*ppbitmap = Gdiplus::Bitmap::FromStream(pStream);
				rval = (*ppbitmap && (*ppbitmap)->GetLastStatus() != Gdiplus::Ok);
				pStream->Release();
			}
		}
		GlobalFree(hGlobTemp);
	}

	return rval;
}

void AddPath(std::vector<string>* paths, LPCWSTR path)
{
	paths->push_back(path);
}

SHSTDAPI_(BOOL) SHGetPathFromIDListW(PCIDLIST_ABSOLUTE pidl, __out_ecount(MAX_PATH) LPWSTR pszPath);

// IShellExtInit
HRESULT CMenu::Initialize(PCIDLIST_ABSOLUTE pidlFolder, IDataObject *pdtobj, HKEY hkeyProgID)
{
	FORMATETC fmt = {CF_HDROP, NULL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL};
	STGMEDIUM medium = {0};
	HDROP hdrop = 0;

	if (!pdtobj)
	{
		wchar_t buffer[MAX_PATH];
		if (!SHGetPathFromIDListW(pidlFolder, buffer))
			return HRESULT_FROM_WIN32(GetLastError());

		AddPath(&m_Paths, buffer);
		return S_OK;
	}

	__try
	{
		HRESULT hr;
		if (FAILED(hr = pdtobj->GetData(&fmt, &medium)))
			return hr;

		hdrop = static_cast<HDROP>(GlobalLock(medium.hGlobal));
		if (!hdrop)
			return HRESULT_FROM_WIN32(GetLastError());

		UINT cFiles = DragQueryFile(hdrop, 0xFFFFFFFF, NULL, 0);
		if (!cFiles)
			return HRESULT_FROM_WIN32(GetLastError());

		for	(UINT ix = 0; ix < cFiles; ix++)
		{
			wchar_t buffer[MAX_PATH];
			if (!DragQueryFileW(hdrop, ix, buffer, MAX_PATH))
				return HRESULT_FROM_WIN32(GetLastError());

			AddPath(&m_Paths, buffer);
		}

		return S_OK;
	}
	__finally
	{
		if (medium.hGlobal)
			GlobalUnfix(medium.hGlobal);
		if (medium.tymed)
			ReleaseStgMedium(&medium);
	}
}

// IContextMenu
HRESULT CMenu::QueryContextMenu(HMENU hmenu, UINT indexMenu, UINT idCmdFirst, UINT idCmdLast, UINT uFlags)
{
	const MenuCommand* pcmd = _Commands;
	UINT id = idCmdFirst;

	std::string status;
	for (std::vector<string>::const_iterator it = m_Paths.begin(); it != m_Paths.end(); it++)
	{
		char buffer[3];
		RETURN_FAILED(HgGetStatus(it->c_str(), buffer, buffer + 1));
		buffer[2] = '\0';

		status.append(buffer);
	}
	
	m_Commands.clear();
	while (pcmd->showstatus)
	{
		if (pcmd->showstatus[strcspn(pcmd->showstatus, status.c_str())])
		{
			MenuCommand cmd = *pcmd;
			cmd.enabled = pcmd->enablestatus[strcspn(pcmd->enablestatus, status.c_str())];
			m_Commands.push_back(cmd);
		}
		pcmd++;
	}

	size_t c = m_Commands.size();
	if (c == 0)
		return MAKE_HRESULT(0, 0, c);

	HMENU hpopup = CreatePopupMenu();
	if (!hpopup)
		return HRESULT_FROM_WIN32(GetLastError());

	for (UINT ix = 0; ix < c; ix++)
	{
		pcmd = &m_Commands[ix];
		if (!pcmd->action)
		{
			InsertMenu(hpopup, ix + idCmdFirst, MF_SEPARATOR | MF_BYPOSITION, 0, NULL); 
			continue;
		}

		MENUITEMINFO mii = {sizeof(mii), 0};
		mii.fMask = MIIM_BITMAP | MIIM_ID | MIIM_DATA | MIIM_STRING | MIIM_STATE;
		mii.wID = ix + idCmdFirst;
		mii.dwItemData = (ULONG_PTR)pcmd;
		mii.dwTypeData = (LPWSTR)pcmd->text;
		mii.fState = pcmd->enabled ? MFS_ENABLED : MFS_DISABLED;

		if (pcmd->icon)
		{
			Gdiplus::Bitmap* bmp;
			if (ChgLoadBitmap(pcmd->icon, &bmp))
			{
				Gdiplus::Bitmap bmpIcon(16, 16);
				
				{ // like a using block in C#
					Gdiplus::Graphics g(&bmpIcon);
					Gdiplus::SolidBrush brush(Gdiplus::Color::White);
					g.FillRectangle(&brush, 0, 0, 16, 16);
					g.DrawImage(bmp, 0, 0, 16, 16);
				}
				delete bmp;

				if (Gdiplus::Ok != bmpIcon.GetHBITMAP(Gdiplus::Color::White, &mii.hbmpItem))
					mii.hbmpItem = NULL;
				mii.fMask = mii.fMask & ~MIIM_BITMAP;
			}
		}

		if (!InsertMenuItem(hpopup, ix + idCmdFirst, TRUE, &mii))
			return HRESULT_FROM_WIN32(GetLastError());
	}

	InsertMenu(hmenu, indexMenu++, MF_SEPARATOR | MF_BYPOSITION, 0, NULL); 

	if (!InsertMenu(hmenu, indexMenu++,
		MF_STRING | MF_BYPOSITION | MF_POPUP, (UINT_PTR)hpopup,
		L"CuteHg"))
		return HRESULT_FROM_WIN32(GetLastError());


	return MAKE_HRESULT(SEVERITY_SUCCESS, 0, c + 2);
}

HRESULT CMenu::InvokeCommand(CMINVOKECOMMANDINFO *pici)
{
	if (pici->cbSize != sizeof(CMINVOKECOMMANDINFOEX))
		return E_INVALIDARG;

	CMINVOKECOMMANDINFOEX* pinfo = (CMINVOKECOMMANDINFOEX*)pici;

	const MenuCommand* pcmd;
	size_t uid = (size_t)pinfo->lpVerb;
	if (uid < m_Commands.size())
		pcmd = &m_Commands[uid];
	else
		return E_NOTIMPL;

	if (!pcmd->action)
		return E_NOTIMPL;

	string path;
	if (!HgFindRoot(m_Paths.at(0).c_str(), &path))
		path = pinfo->lpDirectoryW;
	
	return pcmd->action(path, &m_Paths, pcmd);
}

HRESULT CMenu::GetCommandString(UINT_PTR idCmd, UINT uType, UINT *pReserved, LPSTR pszName, UINT cchMax)
{
	const MenuCommand* pcmd;
	if (idCmd < m_Commands.size())
		return E_INVALIDARG;
	pcmd = &m_Commands[idCmd];

	LPCWSTR result = NULL;

	switch (uType)
	{
	case GCS_HELPTEXTA:
	case GCS_HELPTEXTW:
		result = pcmd->help ? pcmd->help : pcmd->text;
		break;
	default:
		return E_INVALIDARG;
	}

	if (uType & GCS_UNICODE)
		wcsncpy_s((LPWSTR)pszName, cchMax, result, _TRUNCATE);
	else
	{
		if (!WideCharToMultiByte(CP_ACP, 0, result, (int)wcslen(result) + 1, pszName, cchMax, NULL, NULL))
			return HRESULT_FROM_WIN32(GetLastError());
	}
	return S_OK;
}

// IContextMenu2
HRESULT CMenu::HandleMenuMsg(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult;
	return this->HandleMenuMsg2(uMsg, wParam, lParam, &lResult);
}

// IContextMenu3
HRESULT CMenu::HandleMenuMsg2(UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT *plResult)
{
	// For now we don't do anything special
	switch (uMsg)
	{
	case WM_MEASUREITEM:
		{
			MEASUREITEMSTRUCT* lpmis = (MEASUREITEMSTRUCT*)lParam;
			if (lpmis==NULL)
				break;
			lpmis->itemWidth += 2;
			if (lpmis->itemHeight < 16)
				lpmis->itemHeight = 16;
			*plResult = TRUE;
		}
		break;

	case WM_DRAWITEM:
		{
			LPDRAWITEMSTRUCT lpDrawItem = (LPDRAWITEMSTRUCT)lParam;
			UINT x = lpDrawItem->itemID;
		}
		break;
	}
	return S_OK;
}


// ITest
HRESULT CMenu::Test(int a, LPCWSTR pwszMessage, int* pa, LPWSTR* pOut)
{
	size_t len = wcslen(pwszMessage);
	*pa = a * a;
	*pOut = (wchar_t*)malloc((len + 1) * sizeof(wchar_t));

	LPCWSTR psrc = pwszMessage;
	LPWSTR ptarg = *pOut + len;
	(*ptarg--) = L'\0';
	while (*psrc)
		(*ptarg--) = (*psrc++);

	return S_OK;
}


// Utility Stuff

HRESULT CALLBACK TestAction(const string& path, const std::vector<string>* files, const MenuCommand*)
{
	string paths;
	for (std::vector<string>::const_iterator it = files->begin(); it != files->end(); it++)
	{
		if (it != files->begin())
			paths.append(L"\" \"");
		paths.append(*it);
	}

	string title = path;
	title += L" - CuteHg Test";

	wchar_t buffer[MAX_PATH + 100];
	wsprintf(buffer, L"You clicked \"%s\"", paths.c_str());
	MessageBox(NULL, buffer, title.c_str(), MB_OK);
	return S_OK;
}

HRESULT CALLBACK InternalStandardAction(LPCWSTR cmdline, const string& path, const std::vector<string>* files, const MenuCommand* cmd)
{
	string paths;
	for (std::vector<string>::const_iterator it = files->begin(); it != files->end(); it++)
	{
		if (it != files->begin())
			paths.append(L"\" \"");
		paths.append(*it);
	}

	string hg;
	LPCWSTR cmdexe;
	Hg(&hg, &cmdexe);
	if (hg.empty())
		MessageBox(0, L"Cannot find Mercurial, ensure that hg.exe or hg.bat is in your PATH.", L"CuteHg", MB_OK);

	LPCWSTR args[] = 
	{
		hg.c_str(),
		path.c_str(),
		paths.c_str(),
	};

	wchar_t buffer[32767];
	if (!FormatMessage(FORMAT_MESSAGE_FROM_STRING | FORMAT_MESSAGE_ARGUMENT_ARRAY, cmdline, 0, 0, buffer, 32767, (va_list*)args))
		return HRESULT_FROM_WIN32(GetLastError());

	STARTUPINFO si;
    PROCESS_INFORMATION pi;

	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);

	if (!CreateProcess(cmdexe, buffer, NULL, NULL, FALSE, cmd->showConsole ? 0 : CREATE_NO_WINDOW, NULL, path.c_str(), &si, &pi))
		return HRESULT_FROM_WIN32(GetLastError());

	CloseHandle(pi.hThread);
	CloseHandle(pi.hProcess);

	return S_OK;
}

HRESULT CALLBACK StandardCuteAction(const string& path, const std::vector<string>* files, const MenuCommand* cmd)
{
	string cmdline;

	cmdline.append(L"\"%1\" ");
	cmdline.append(cmd->verb);
	cmdline.append(L" \"%3\"");

	return InternalStandardAction(
		cmdline.c_str(),
		path, files, cmd);
}

HRESULT CALLBACK StandardCuteNoFilesAction(const string& path, const std::vector<string>* files, const MenuCommand* cmd)
{
	string cmdline;

	cmdline.append(L"\"%1\" ");
	cmdline.append(cmd->verb);

	return InternalStandardAction(
		cmdline.c_str(),
		path, files, cmd);
}


HRESULT CALLBACK StandardAction(const string& path, const std::vector<string>* files, const MenuCommand* cmd)
{
	return InternalStandardAction(cmd->cmdline, path, files, cmd);
}
