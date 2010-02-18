
// Copyright (C) 2009 Benjamin Pollack 
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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <tchar.h>

#ifndef WIN32

#include <string.h>
#include <Winsock2.h>
#define MAX_PATH          260

#define w_pathcmp _tcsicmp
#define a_pathcmp _stricmp
#define w_pathncmp _tcsnicmp
#define a_pathncmp _strnicmp
#define FOPEN_S(pf, f) _wfopen_s(&pf, f, L"rb")

static __int64 secs_between_epochs = 11644473600; /* Seconds between 1.1.1601 and 1.1.1970 */

int lstat(const TCHAR* file, struct _stat* pstat)
{
	WIN32_FIND_DATA data;
	HANDLE hfind;
	__int64 temp;

	hfind = FindFirstFile(file, &data);
	if (hfind == INVALID_HANDLE_VALUE)
		return -1;
	FindClose(hfind);

	pstat->st_mtime = *(__int64*)&data.ftLastWriteTime / 10000000 - secs_between_epochs;
	pstat->st_size = (data.nFileSizeHigh << sizeof(data.nFileSizeHigh)) | data.nFileSizeLow;

	return 0;
}

#else

#include <strings.h>
#include <arpa/inet.h>
#define MAX_PATH          260

#define TCHAR char
#define w_pathcmp _strcmp
#define a_pathcmp _strcmp
#define w_pathncmp _strncmp
#define a_pathncmp _strncmp
#define FOPEN_S(pf, f) fopen_s(&pf, f, "r")

#endif

#define HASH_LENGTH 20

typedef struct _direntry
{
    unsigned char state;
    unsigned mode;
    unsigned size;
    unsigned mtime;
    unsigned length;
    char *name;
    char *origname;
} direntry;

typedef struct _dirstate
{
    char parent1[HASH_LENGTH];
    char parent2[HASH_LENGTH];
    
    unsigned num_entries;
    direntry *entries;
    unsigned __entries_length;
} dirstate;

typedef struct _dirstatecache
{
	const dirstate* dirstate;
	struct _dirstatecache* next;
	__time64_t mtime;
	TCHAR path[MAX_PATH];
} dirstatecache;

void *xalloc(size_t n, size_t size)
{
    void *p = calloc(n, size);
    if (!p) exit(1);
    return p;
}

void dirstate_add_entry(dirstate *pd, const direntry *pe)
{
    if (pd->num_entries == pd->__entries_length)
    {
        pd->__entries_length = pd->__entries_length ? 2 * pd->__entries_length : 1;
        pd->entries = realloc(pd->entries, pd->__entries_length * sizeof(direntry));
    }
    pd->entries[pd->num_entries++] = *pe;
}

dirstate *dirstate_new(const TCHAR *path)
{
    direntry e;
    FILE *f;
	dirstate *pd;
		
    if (0 != FOPEN_S(f, path)) return NULL;
    pd = (dirstate*)xalloc(1, sizeof(dirstate));
    fread(&pd->parent1, sizeof(char), HASH_LENGTH, f);
    fread(&pd->parent2, sizeof(char), HASH_LENGTH, f);

    while (fread(&e.state, sizeof(e.state), 1, f) == 1)
    {
        e.name = e.origname = 0;
        
        fread(&e.mode, sizeof(e.mode), 1, f);
        fread(&e.size, sizeof(e.size), 1, f);
        fread(&e.mtime, sizeof(e.mtime), 1, f);
        fread(&e.length, sizeof(e.length), 1, f);

        e.mode = ntohl(e.mode);
        e.size = ntohl(e.size);
        e.mtime = ntohl(e.mtime);
        e.length = ntohl(e.length);
        
        e.name = malloc(e.length * sizeof(char) + 1);
        fread(e.name, sizeof(char), e.length, f);
        e.name[e.length] = 0;
        dirstate_add_entry(pd, &e);
    }

	fclose(f);
    
    return pd;
}

void dirstate_free(const dirstate *pd)
{
    unsigned ix;
    for (ix = 0; ix < pd->num_entries; ++ix) free(pd->entries[ix].name);
    free(pd->entries);
    free((void*)pd);
}

dirstatecache* _cache = NULL;
const dirstate* dirstate_get(const TCHAR* hgroot)
{
	TCHAR path[MAX_PATH];
	struct _stat stat;
	dirstatecache* head;

	_tcscpy_s(path, MAX_PATH, hgroot);
	_tcscat_s(path, MAX_PATH, _T("/.hg/dirstate"));

	if (0 != lstat(path, &stat))
		return NULL;

	head = _cache;
	while (head)
	{
		if (w_pathcmp(path, head->path) == 0)
			break;
		head = head->next;
	}

	if (!head)
	{
		head = (dirstatecache*)xalloc(1, sizeof(dirstatecache));
		head->next = _cache;
		_cache = head;
		_tcscpy_s(head->path, MAX_PATH, path);
	}

	if (head->mtime < stat.st_mtime)
	{
		head->mtime = stat.st_mtime;
		if (head->dirstate)
			dirstate_free(head->dirstate);
		head->dirstate = dirstate_new(path);
	}

	return head->dirstate;
}

static char *revhash_string(const char revhash[HASH_LENGTH])
{
    unsigned ix;
    static char rev_string[HASH_LENGTH * 2 + 1];
    static char *hexval = "0123456789abcdef";
    for (ix = 0; ix < HASH_LENGTH; ++ix)
    {
        rev_string[ix * 2] = hexval[(revhash[ix] >> 4) & 0xf];
        rev_string[ix * 2 + 1] = hexval[revhash[ix] & 0xf];
    }
    rev_string[sizeof(rev_string)] = 0;
    return rev_string;
}

char mapdirstate(const direntry* entry, const struct _stat* stat)
{
	switch (entry->state)
	{
	case 'n':
		if (entry->mtime == (unsigned)stat->st_mtime
			&& entry->size == (unsigned)stat->st_size
#ifndef WIN32
			&& entry->mode == stat->st_mode
#endif
			)
			return 'C';
		else
			return 'M';
	case 'm':
		return 'M';
	case 'r':
		return 'R';
	case 'a':
		return 'A';
	default:
		return '?';
	}
}

int HgQueryDirstate(const TCHAR* hgroot, const TCHAR* abspath, char* relpathloc, const dirstate** ppd, 	struct _stat* pstat)
{
	char* temp;

	if (0 != lstat(abspath, pstat))
		return 0;

	*ppd = dirstate_get(hgroot);
	if (!*ppd)
		return 0;

	temp = relpathloc;
	while (*temp)
	{
		if (*temp == '\\')
			*temp = '/';
		temp++;
	}

	return 1;
}

int HgQueryDirstateDirectory(const TCHAR* hgroot, const TCHAR* abspath, char* relpathloc, char* outStatus)
{
	const dirstate* pd;
	struct _stat stat;
	unsigned ix;
	size_t len, rootlen;
	TCHAR temp[2*MAX_PATH];
	int a = 0, m = 0;

	if (!HgQueryDirstate(hgroot, abspath, relpathloc, &pd, &stat))
		return 0;

	rootlen = _tcslen(hgroot);
	len = strlen(relpathloc);
	for (ix = 0; ix < pd->num_entries && !a; ix++)
	{
		if (0 != a_pathncmp(relpathloc, pd->entries[ix].name, len))
			continue;

		switch (pd->entries[ix].state)
		{
		case 'n':
			if (!m)
			{
				_tcscpy_s(temp, MAX_PATH, hgroot);
				_tcscat_s(temp, MAX_PATH, _T("/"));
#ifdef WIN32
				MultiByteToWideChar(CP_UTF8, 0, pd->entries[ix].name, strlen(pd->entries[ix].name) + 1, temp + rootlen + 1, MAX_PATH);
#else
				_tcscat_s(temp, MAX_PATH, pd->entries[ix].name);
#endif
				if (0 == lstat(temp, &stat))
					m = mapdirstate(&pd->entries[ix], &stat) == 'M';
			}
			break;
		case 'm':
			m = 1;
			break;
		case 'a':
			a = 1;
			break;
		}
	}

	if (a)
		*outStatus = 'A';
	else if (m)
		*outStatus = 'M';
	else
		*outStatus = 'C';

	return 1;
}

int HgQueryDirstateFile(const TCHAR* hgroot, const TCHAR* abspath, char* relpathloc, char* outStatus)
{
	const dirstate* pd;
	struct _stat stat;
	unsigned ix;

	if (!HgQueryDirstate(hgroot, abspath, relpathloc, &pd, &stat))
		return 0;

	for (ix = 0; ix < pd->num_entries; ix++)
	{
		if (0 == a_pathcmp(relpathloc, pd->entries[ix].name))
		{
			*outStatus = mapdirstate(&pd->entries[ix], &stat);
			return *outStatus != '?';
		}
	}

	return 0;
}

#if 0
int main(int argc, char *argv[])
{
    dirstate *pd = dirstate_new(".hg/dirstate");
    time_t t;
    char *s;
    unsigned ix;
    printf("parent1: %s\n", revhash_string(pd->parent1));
    printf("parent2: %s\n", revhash_string(pd->parent2));
    printf("entries: %d\n\n", pd->num_entries);
    for (ix = 0; ix < pd->num_entries; ++ix)
    {
        t = pd->entries[ix].mtime;
        s = ctime(&t);
        s[strlen(s) - 1] = '\0';
        printf("%s %s\n", s, pd->entries[ix].name);
    }
    dirstate_free(pd);
    return 0;
}
#endif