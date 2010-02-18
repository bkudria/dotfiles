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

#include "common.h"

ClassEntry::ClassEntry(const CLSID clsid, IClassFactory* (STDMETHODCALLTYPE *GetClassFactory)(), HRESULT (STDMETHODCALLTYPE *UpdateRegistry)(BOOL))
{
	this->next = _entries;
	_entries = this;
	this->clsid = clsid;
	this->GetClassFactory = GetClassFactory;
	this->UpdateRegistry = UpdateRegistry;
}