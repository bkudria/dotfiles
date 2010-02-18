#!/usr/bin/env python
# -*- coding: utf-8 -*-

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Tom Burdick <thomas.burdick@gmail.com> 
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

import re
import urlparse
import urllib
import urllib2
import telnetlib
from mercurial import hg, ui, commands, node, cmdutil, url

'''
Commands are wrappers for mercurial commands that give them a common interface.

The goal is to catch errors before the command is run so that the GUI can notify the user before they even run the command there is some problem.
Another goal here is to attempt to encapsulate the commands in a common interface that can be used, making building on top easier.
'''

class Command(object):
    '''
    base class for all mercurial commands. represents the common
    interface. It is expected needed parameters are passed in the constructor
    '''
    def test(self):
        '''
        tests the command, throw exceptions for errors that can be caught
        before the actual command is performed, this gives the UI a chance
        to query the user, if there are options, on what they'd like to do
        '''
        raise NotImplemented()

    def perform(self):
        '''
        perform the actual command, may throw an exception still
        even if the command can be done as told by canPerform
        '''
        raise NotImplemented()

    def revert(self):
        '''
        some commands can be "undone", this is the call that will do that
        '''
        raise NotImplemented()

    def __call__(self):
        '''
        Command and derivatives are all callable, the call is redirected
        to first test, then perform. This allows for Command objects to act as callbacks.
        '''
        self.test()
        self.perform()

class CommandException(Exception):
    pass

def test_uncommitted_merge(repo):
    ''' test to see if there is an uncommitted merge 
        return True if a merge has been uncommitted
        return False otherwise 
    '''
    parent1, parent2 = repo.dirstate.parents()
        
    if parent2 != node.nullid:
        return True

    return False
     
def test_missing_files(repo):
    ''' test to see if there are missing files
        return True if there are missing files
        return False otherwise
    '''
    missing = None 
    wlock = lock = None
    try:
        wlock = repo.wlock()
        lock = repo.lock()
        missing = repo.status()[4]
    finally:
        del lock, wlock

    if missing:
        return True
    return False

def test_uncommitted_changes(repo):
    ''' test to see if there are uncommitted changes
        return True if there are uncommitted changes
        return False otherwise
    '''
    modified = None
    added = None 
    removed = None 

    wlock = lock = None
    try:
        wlock = repo.wlock()
        lock = repo.lock()
        modified, added, removed = repo.status()[:3]
    finally:
        del lock, wlock

    if modified or added or removed:
        return True
    return False

class UncommittedChangesError(CommandException):
    def __init__(self, **args):
        CommandException.__init__(self, args)
    def __str__(self):
        return "uncommitted changes"

class UncommittedMergeError(CommandException):
    def __init__(self, **args):
        CommandException.__init__(self, args)
    def __str__(self):
        return "uncommitted merge"

class MissingFilesError(CommandException):
    def __init__(self, **args):
        CommandException.__init__(self, args)
    def __str__(self):
        return "missing files"

class Update(Command):
    '''
    Wrapper for mercurial commands.update
    '''
    def __init__(self, ui, repo, rev, force=False):
        self._ui = ui
        self._repo = repo
        self._rev = rev
        self.force = force

    def test(self):
        if test_uncommitted_merge(self._repo):
            raise UncommittedMergeError 
        if test_uncommitted_changes(self._repo):
            raise UncommittedChangesError
        if test_missing_files(self._repo):
            raise MissingFilesError
    
    def perform(self):
        commands.update(self._ui, self._repo, self._rev, self.force)

def test_local_path(url):
    ''' test if the url is a local path or not
        return True if the url is a local filesystem path
        return False otherwise
    '''

    unixfolderRegExp = re.compile('^/.*')
    windowsfolderRegExp = re.compile('^[A-Za-z]:[\\/].*')
    fileRegExp = re.compile('^[fF][iI][lL][eE]://.*')

    if unixfolderRegExp.match(url) or windowsfolderRegExp.match(url) or fileRegExp.match(url):
        return True
    else:
        return False

def test_http_path(url):
    ''' test if the url is a http path or not
        return True if the url is a http path
        return False otherwise
    '''
    httpRegExp = re.compile('^[hH][tT][tT][pP]://.*')

    if httpRegExp.match(url):
        return True
    return False

def test_https_path(url):
    ''' test if the url is a https path or not
        return True if the url is a https path
        return False otherwise
    '''
    httpsRegExp = re.compile('^[hH][tT][tT][pP][sS]://.*')

    if httpsRegExp.match(url):
        return True
    return False

def test_ssh_path(url):
    ''' test if the url is a ssh path or not
        return True if the url is a ssh path
        return False otherwise
    '''
    sshRegExp = re.compile('^[sS][sS][hH]://.*')

    if sshRegExp.match(url):
        return True
    return False

def test_remote_connect(hostname, port):
    ''' test if the host and port can be connected to using telnet (should work for all hg protocols currently)
        return True if the urlobject (urlparse results) can be connected to
        return False otherwise
    '''
    result = False

    try:
        session = telnetlib.Telnet(hostname, port)
        result = True
    except:
        pass    
    finally:
        session.close()
    
    return result

def test_ssh_auth(hostname, port, username, password):
    ''' test if the host, port, username, and password correctly authenticate using ssh
        return True if the host, port, username, and password correctly authenticate
        return False otherwise
    '''
    import paramiko
    from paramiko import AutoAddPolicy, SSHClient
    try:
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        client.connect(hostname, port, username=username, password=password, timeout=3)
        stdin, stdout, stderr = client.exec_command('ls')
        client.close()
        return True
    except Exception, e:
        try:
            t.close()
        except:
            pass
        return False

def test_http_auth(ui, path):
    ''' test if the host, port, username, and password correctly authenticate using http/https
        return True if the host, port, username, and password correctly authenticate
        return False otherwise
    '''
    # urllib cannot handle URLs with embedded user or passwd
    murl, authinfo = url.getauthinfo(path)
    urlopener = url.opener(ui, authinfo)
    data = None
    headers = {}
    q = {"cmd": "heads"}
    qs = '?%s' % urllib.urlencode(q)
    cu = "%s%s" % (murl, qs)
    try:
        resp = urlopener.open(urllib2.Request(cu, data, headers))
        return True
    except Exception, e:
        print "%s: %s" % (e.__class__.__name__, e)
        return False

def test_push_new_head(repo, remoterepo, revs, force):
    ''' test to see if new heads will be created when pushing
        return True if new heads will be created
        return False otherwise
    '''
    if revs:
        revs = [repo.lookup(rev) for rev in revs]
    common = {}
    remote_heads = remoterepo.heads()
    inc = repo.findincoming(remoterepo, common, remote_heads, force=force)

    update, updated_heads = repo.findoutgoing(remoterepo, common, remote_heads)
    if revs is not None:
        msng_cl, bases, heads = repo.changelog.nodesbetween(update, revs)
    else:
        bases, heads = update, repo.changelog.heads()
        
    if remote_heads == [nullid]:
        warn = 0
    elif not revs and len(heads) > len(remote_heads):
        warn = 1
    else:
        newheads = list(heads)
        for r in remote_heads:
            if r in repo.changelog.nodemap:
                desc = repo.changelog.heads(r, heads)
                l = [h for h in heads if h in desc]
                if not l:
                    newheads.append(r)
            else:
                newheads.append(r)
        if len(newheads) > len(remote_heads):
            warn = 1
    if warn:
        return True
    return False

def test_push_nochanges(repo, remoterepo, revs, force):
    ''' test to see if there are no changes to push to the remote repo
        return True if no changes need to be pushed
        return False otherwise
    '''
    if revs:
        revs = [repo.lookup(rev) for rev in revs]
    common = {}
    remote_heads = remoterepo.heads()
    inc = repo.findincoming(remoterepo, common, remote_heads, force=force)

    update, updated_heads = repo.findoutgoing(remoterepo, common, remote_heads)
    if revs is not None:
        msng_cl, bases, heads = repo.changelog.nodesbetween(update, revs)
    else:
        bases, heads = update, repo.changelog.heads()

    if not bases:
        return True

    return False

def test_pull_nochanges(repo, remoterepo, revs, force):
    ''' test to see if there are no changes to pull from the remote repo
        return True if no changes need to be pulled
        return False otherwise
    '''
    if revs:
        revs = [remote.lookup(rev) for rev in revs]

    common, fetch, rheads = repo.findcommonincoming(remoterepo, heads=revs, force=force)

    if not fetch:
        return True

    return False


class PathError(CommandException):
    ''' raise when the url is a local folder and it doesn't exist '''
    def __init__(self, **args):
        CommandException.__init__(self, args)
    
    def __str__(self):
        return "path does not exist"

class UrlError(CommandException):
    ''' raised when a url is invalid
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)
        
    def __str__(self):
        return "invalid URL"

class RemoteConnectError(CommandException):
    ''' raised when a remote host can't be contacted
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "failed to connect to remote host"
        
class AuthError(CommandException):
    ''' raised when authenticating fails
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "failed to authenticate, invalid user or password"
        
class RemoteRepoError(CommandException):
    ''' raised when the remote repository isn't valid
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "URL does not point to a mercurial repository"

class NewRemoteHeadsError(CommandException):
    ''' raised when pushing will cause new heads to come about on the remote repository
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "pushing creates new remote heads"

class NoPushChangesError(CommandException):
    ''' raised when there are no local changes to push
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "no changes needed to be pushed"

class NoPullChangesError(CommandException):
    ''' raised when there are no local changes to push
    '''
    def __init__(self, **args):
        CommandException.__init__(self, args)

    def __str__(self):
        return "no changes needed to be pulled"


class Push(Command):
    '''
    Wrapper for mercurial commands.push
    '''
    
    def __init__(self, ui, repo, url, rev=None, force=False):
        self._ui = ui
        self._repo = repo
        self._url = url
        self._rev = rev
        self.force = force

    def test(self):
        ''' test pushing '''

        if test_local_path(self._url):
            if test_local_path_exists(self._url):
                raise PathError
                
        elif test_http_path(self._url) or test_https_path(self._url):
            urlresult = urlparse.urlsplit(self._url)

            if urlresult.scheme == '':
                raise UrlError
            if urlresult.hostname == '':
                raise UrlError
            
            hostname = urlresult.hostname
            port = urlresult.port
            username = urlresult.username
            password = urlresult.password 

            if not port:
                if urlresult.scheme == 'http':
                    port = 80
                elif urlresult.scheme == 'https':
                    port = 443

            if not test_remote_connect(hostname, port):
                raise RemoteConnectError

            if username and password:
                if not test_http_auth(self._ui, self._url):
                    raise AuthError

        elif test_ssh_path(self._url):
            print "url is ssh"
            urlparse.uses_netloc.append('ssh')
            urlresult = urlparse.urlsplit(self._url)
            
            hostname = urlresult.hostname
            port = urlresult.port
            username = urlresult.username
            password = urlresult.password 
         
            if urlresult.scheme == '':
                raise UrlError
            if urlresult.hostname == '':
                raise UrlError
            
            hostname = urlresult.hostname
            port = urlresult.port
            if not port:
                port = 22

            print "testing remote connect"
            if not test_remote_connect(hostname, port):
                raise RemoteConnectError
            print "done testing remote connect"

            if username:
                print "testing auth"
                if not test_ssh_auth(hostname, port, username, password):
                    raise AuthError
            print "done testing auth"
        else:
            raise UrlError

        # finally test the repository using mercurial itself to determine if #1 its valid, #2 there are changes
        dest, revs, checkout = hg.parseurl( self._ui.expandpath(self._url or 'default-push', self._url or 'default'), self._rev)
        opts = {'rev': self._rev, 'force': self.force}
        cmdutil.setremoteconfig(self._ui, opts)
        remoterepo = None

        try:
            remoterepo = hg.repository(self._ui, self._url)
        except:
            raise RemoteRepoError
        
        if test_push_nochanges(self._repo, remoterepo, revs, self.force):
            raise NoPushChangesError
        
    def perform(self):
        ''' actually perform the push '''
        opts = {'rev':self._rev, 'force':self.force}
        commands.push(self._ui, self._repo, self._url)


class Pull(Command):
    '''
    Wrapper for mercurial commands.pull
    '''

    def __init__(self, ui, repo, url, rev=None, force=False):
        self._ui = ui
        self._repo = repo
        self._url = url
        self._rev = rev
        self.force = force

    def test(self):
        ''' test pulling '''
       
        if test_local_path(self._url):
            if test_local_path_exists(self._url):
                raise PathError
                
        elif test_http_path(self._url) or test_https_path(self._url):
            urlresult = urlparse.urlsplit(self._url)

            if urlresult.scheme == '':
                raise UrlError
            if urlresult.hostname == '':
                raise UrlError
            
            hostname = urlresult.hostname
            port = urlresult.port
            username = urlresult.username
            password = urlresult.password 

            if not port:
                if urlresult.scheme == 'http':
                    port = 80
                elif urlresult.scheme == 'https':
                    port = 443

            if not test_remote_connect(hostname, port):
                raise RemoteConnectError

            if username and password:
                if not test_http_auth(self._ui, self._url):
                    raise AuthError

        elif test_ssh_path(self._url):
            urlparse.uses_netloc.append('ssh') #a bit of a hack to ensure ssh is a scheme
            urlresult = urlparse.urlsplit(self._url)
            
            if urlresult.scheme == '':
                raise UrlError
            if urlresult.hostname == '':
                raise UrlError

            hostname = urlresult.hostname
            port = urlresult.port
            username = urlresult.username
            password = urlresult.password 
            
            if not port:
                port = 22

            if not test_remote_connect(hostname, port):
                raise RemoteConnectError

            if username:
                if not test_ssh_auth(hostname, port, username, password):
                    raise AuthError
        else:
            raise UrlError

        # finally test the repository using mercurial itself to determine if #1 its valid, #2 there are changes
        dest, revs, checkout = hg.parseurl( self._ui.expandpath(self._url or 'default-push', self._url or 'default'), self._rev)
        opts = {'rev': self._rev, 'force': self.force}
        cmdutil.setremoteconfig(self._ui, opts)
        remoterepo = None

        try:
            remoterepo = hg.repository(self._ui, self._url)
        except:
            raise RemoteRepoError
        
        if test_pull_nochanges(self._repo, remoterepo, revs, self.force):
            raise NoPullChangesError

    def perform(self):
        ''' actually perform the pull '''
        commands.pull(self._ui, self._repository, self._url, {'rev':self._rev, 'force':self.force})


class Merge(Command):
    '''
    Wrapper for mercurial commands.merge
    '''

class Tag(Command):
    '''
    Wrapper for mercurial commands.tag
    '''

class Branch(Command):
    '''
    Wrapper for mercurial commands.branch
    '''

class Commit(Command):
    '''
    Wrapper for mercurial commands.commit
    '''


