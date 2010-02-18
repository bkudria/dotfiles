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

//-----------------------------------------------------------------------------
//
// This file exists because the VC Express versions do not contian ATL
//
//-----------------------------------------------------------------------------

#define ATL_NO_VTABLE __declspec(novtable)
#define DECLARE_CLASSFACTORY()
#define DECLARE_NOT_AGGREGATABLE(cls)

#define DECLARE_LIBID(libid)
#define DECLARE_REGISTRY_APPID_RESOURCEID(idr, szlibid)

const GUID CLSID_ATLRegistrar = { 0x44ec053a, 0x400f,0x11d0, {0x9d,0xcd,0x00,0xa0,0xc9,0x03,0x91,0xd3} };

struct ClassEntry
{
	ClassEntry(const CLSID clsid, IClassFactory* (STDMETHODCALLTYPE *GetClassFactory)(), HRESULT (STDMETHODCALLTYPE *UpdateRegistry)(BOOL));
	const ClassEntry* next;
	CLSID clsid;
	IClassFactory* (STDMETHODCALLTYPE *GetClassFactory)();
	HRESULT (STDMETHODCALLTYPE *UpdateRegistry)(BOOL);
};
__declspec(selectany) const ClassEntry* _entries = NULL;
#define OBJECT_ENTRY_AUTO(clsid, cls) \
	__declspec(selectany) ClassEntry __COCLASS_##cls##_ENTRY(clsid, cls::GetClassFactory, cls::UpdateRegistry)

struct _ATL_REGMAP_ENTRY
{
	LPCWSTR m_Key;
	LPCWSTR m_Value;
};

template <class T>
class CAtlDllModuleT
{
public:
	BOOL DllMain(DWORD dwReason, LPVOID lpReserved)
	{
		return TRUE;
	}

	HRESULT DllCanUnloadNow()
	{
		return S_FALSE;
	}

	HRESULT DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv)
	{
		const ClassEntry* entry = _entries;
		while (entry)
		{
			if (rclsid == entry->clsid)
			{
				*ppv = entry->GetClassFactory();
				return S_OK;
			}
			entry = entry->next;
		}
		return E_INVALIDARG;
	}

	HRESULT DllRegisterServer(BOOL)
	{
		HRESULT hr = S_OK;
		const ClassEntry* entry = _entries;
		while (entry && SUCCEEDED(hr))
		{
			hr = entry->UpdateRegistry(TRUE);
			entry = entry->next;
		}
		return hr;
	}

	HRESULT DllUnregisterServer()
	{
		HRESULT hr = S_OK;
		const ClassEntry* entry = _entries;
		while (entry)
		{
			hr = entry->UpdateRegistry(FALSE);
			entry = entry->next;
		}
		return S_OK;
	}

	HRESULT UpdateRegistryFromResourceS(UINT idr, BOOL bRegister, _ATL_REGMAP_ENTRY* entries)
	{
		IRegistrar* pReg;

		//DebugBreak();

		HRESULT hr = CoCreateInstance(CLSID_ATLRegistrar, NULL, CLSCTX_ALL, __uuidof(IRegistrar), (void**)&pReg);
		if (FAILED(hr))
			return hr;

		wchar_t path[32000];
		GetModuleFileNameW(_hInstance, path, 32000);

		pReg->ClearReplacements();
		pReg->AddReplacement(L"MODULE", path);
		if (entries)
		{
			while (entries->m_Key)
			{
				pReg->AddReplacement(entries->m_Key, entries->m_Value);
				entries++;
			}
		}

		hr = bRegister
			? pReg->ResourceRegister(path, idr, L"REGISTRY")
			: pReg->ResourceUnregister(path, idr, L"REGISTRY");

		pReg->Release();
		return hr;
	}
};

template <class coclass>
class CCoClassFactory
	: IClassFactory
{
public:
	virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, void **ppvObject)
	{
		if (riid == IID_IUnknown)
		{
			*ppvObject = (void*)(IUnknown*)this;
			return S_OK;
		}
		if (riid == IID_IClassFactory)
		{
			*ppvObject = (void*)(IClassFactory*)this;
			return S_OK;
		}
		return E_NOINTERFACE;
	}

	virtual ULONG STDMETHODCALLTYPE AddRef()
	{
		return 1;
	}

	virtual ULONG STDMETHODCALLTYPE Release()
	{
		return 1;
	}

	virtual HRESULT STDMETHODCALLTYPE CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppvObject)
	{
		if (pUnkOuter != NULL)
			return E_INVALIDARG;
		CComObject<coclass>* punk = new CComObject<coclass>();
		punk->AddRef();
		
		HRESULT hr = punk->QueryInterface(riid, ppvObject);
		if (SUCCEEDED(hr))
		{
			hr = punk->Construct();
			if (FAILED(hr))
				punk->Release();
		}

		punk->Release();
		return hr;
	}

	virtual HRESULT STDMETHODCALLTYPE LockServer(BOOL fLock)
	{
		return S_OK;
	}
};

class CComSingleThreadModel
{
public:
	static ULONG Increment(ULONG& ul)
	{
		return ++ul;
	}

	static ULONG Decrement(ULONG& ul)
	{
		return --ul;
	}
};

template <class base>
class CComObject : public base
{
	BOOL m_constructed;

protected:
	virtual ~CComObject()
	{
		if (m_constructed)
			FinalRelease();
	}

public:
	CComObject() : m_constructed(FALSE)
	{

	}

	virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, void **ppvObject)
	{
		return InternalQueryInterface(riid, ppvObject);
	}

	virtual ULONG STDMETHODCALLTYPE AddRef()
	{
		return InternalAddRef();
	}

	virtual ULONG STDMETHODCALLTYPE Release()
	{
		ULONG result =  InternalRelease();
		if (result == 0)
		{
			delete this;
		}
		return result;
	}

	HRESULT Construct()
	{
		m_constructed = TRUE;
		return FinalConstruct();
	}
};

template <class model>
class ATL_NO_VTABLE CComObjectRootEx
{
	ULONG m_refcnt;
protected:
	virtual ULONG STDMETHODCALLTYPE InternalAddRef()
	{
		return model::Increment(m_refcnt);
	}

	virtual ULONG STDMETHODCALLTYPE InternalRelease()
	{
		return  model::Decrement(m_refcnt);
	}

	virtual HRESULT FinalConstruct()
	{
		return S_OK;
	}

	virtual void FinalRelease()
	{
	}
};

template <class cls, const CLSID* riid>
class ATL_NO_VTABLE CComCoClass
{
public:
	static IClassFactory* STDMETHODCALLTYPE GetClassFactory()
	{
		static CCoClassFactory<cls> Factory;
		return (IClassFactory*)&Factory;
	}
};

#define DECLARE_REGISTRY_RESOURCEID(idr) \
	static HRESULT STDMETHODCALLTYPE UpdateRegistry(BOOL bRegister) \
	{ return _AtlModule.UpdateRegistryFromResourceS(idr, bRegister, NULL); }

#define BEGIN_COM_MAP(cls) HRESULT InternalQueryInterface(REFIID riid, void **ppvObject) {
#define COM_INTERFACE_ENTRY(iface) if (riid == __uuidof(iface) || riid == IID_IUnknown) { *ppvObject = (void*)(iface*)this; return S_OK; }
#define END_COM_MAP() return E_NOINTERFACE; };
