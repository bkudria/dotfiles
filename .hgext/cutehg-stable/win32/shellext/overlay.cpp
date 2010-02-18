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

template <class cls, const CLSID* rclsid, char status>
class ATL_NO_VTABLE COverlayBase :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<cls, rclsid>, 
	public IShellIconOverlayIdentifier,
	public ITest
{
public:
	DECLARE_CLASSFACTORY();
	DECLARE_NOT_AGGREGATABLE(cls);

	static HRESULT STDMETHODCALLTYPE UpdateRegistry(BOOL bRegister);

	BEGIN_COM_MAP(cls)
	   COM_INTERFACE_ENTRY(IShellIconOverlayIdentifier)
	   COM_INTERFACE_ENTRY(ITest)
	END_COM_MAP()

	// IShellIconOverlayIdentifier
    virtual HRESULT STDMETHODCALLTYPE IsMemberOf(LPCWSTR pwszPath, DWORD dwAttrib);
    virtual HRESULT STDMETHODCALLTYPE GetOverlayInfo(LPWSTR pwszIconFile, int cchMax, int *pIndex, DWORD *pdwFlags);
	virtual HRESULT STDMETHODCALLTYPE GetPriority(int *pIPriority);

	// ITest
	virtual HRESULT STDMETHODCALLTYPE Test(int a, LPCWSTR pwszMessage, int* pa, LPWSTR* pOut);
};

#define DECLARE_OVERLAY(name, status)										\
class ATL_NO_VTABLE C##name##Overlay:										\
	public COverlayBase<C##name##Overlay, &CLSID_CuteHgIcon##name, status>	\
{ public: static LPCWSTR GetName() { return L#name; } };					\
OBJECT_ENTRY_AUTO(CLSID_CuteHgIcon##name, C##name##Overlay);

DECLARE_OVERLAY(Added, 'A');
DECLARE_OVERLAY(Ignored, 'I');
DECLARE_OVERLAY(Normal, 'C');
DECLARE_OVERLAY(Modified, 'M');
DECLARE_OVERLAY(Unversioned, '?');

template <class cls, const CLSID* rclsid, char status>
HRESULT COverlayBase<cls, rclsid, status>::UpdateRegistry(BOOL bRegister)
{
	LPOLESTR sclsid;
	StringFromCLSID(*rclsid, &sclsid);
	
	_ATL_REGMAP_ENTRY entries[] = 
	{
		{_T("CLSID"), sclsid},
		{_T("NAME"), cls::GetName()},
		{NULL, NULL}
	};

	__try
	{
		return _AtlModule.UpdateRegistryFromResourceS(IDR_OVERLAY, bRegister, entries);
	}
	__finally
	{
		CoTaskMemFree(sclsid);
	}
}

// IShellIconOverlayIdentifier
template <class cls, const CLSID* rclsid, char status>
HRESULT COverlayBase<cls, rclsid, status>::IsMemberOf(LPCWSTR pwszPath, DWORD dwAttrib)
{
	char filestatus, childstatus;

	if (!pwszPath)
		return S_FALSE;

	if (FAILED(HgGetStatus(pwszPath, &filestatus, &childstatus)))
		return S_FALSE;

	if (status != filestatus && status != childstatus)
		return S_FALSE;
	
	return S_OK;
}

template <class cls, const CLSID* rclsid, char status>
HRESULT COverlayBase<cls, rclsid, status>::GetOverlayInfo(LPWSTR pwszIconFile, int cchMax, int *pIndex, DWORD *pdwFlags)
{
	return S_OK;
}

template <class cls, const CLSID* rclsid, char status>
HRESULT COverlayBase<cls, rclsid, status>::GetPriority(int *pIPriority)
{
	*pIPriority = 0;
	return S_OK;
}


// ITest
template <class cls, const CLSID* rclsid, char status>
HRESULT COverlayBase<cls, rclsid, status>::Test(int a, LPCWSTR pwszMessage, int* pa, LPWSTR* pOut)
{
	return E_NOTIMPL;
}

