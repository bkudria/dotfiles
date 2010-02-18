# localbranch.py: temporary branches
#
# Copyright 2007-2009 Brendan Cully <brendan@kublai.com>
#
# This software may be used and distributed according to the terms
# of the GNU General Public License, incorporated herein by reference.

from mercurial.i18n import _
from mercurial import hg, encoding, store, util
from mercurial.node import *

import os, shutil

# force import errors immediately
store.store
# 1.2.1 fallback
try:
    encoding.tolocal
except ImportError:
    encoding = util

class localbranchrepo(object):
    def instance(ui, path, create):
        path = util.drop_scheme('lbranch', path)
        if not hasattr(ui, 'repo_path'):
            raise util.Abort('local branch parent not available')
        repo = hg.repository(ui, ui.repo_path)
        repo.loadlocalbranch(path)
        return repo
    instance = staticmethod(instance)

hg.schemes['lbranch'] = localbranchrepo

def reposetup(ui, repo):
    if not repo.local():
        return

    class lbrepo(repo.__class__):
        def getrequirements(self):
            requirements = []
            try:
                requirements = self.opener("requires").read().splitlines()
                for r in requirements:
                    if r not in self.supported:
                        raise repo.RepoError(_("requirement '%s' not supported") % r)
            except IOError, inst:
                if inst.errno != errno.ENOENT:
                    raise

            return requirements

        def localbranchpath(self, name):
            if name == 'default':
                name = ''
            if name:
                bdir = self.join('branches')
                return os.path.join(bdir, store.encodefilename(name))
            else:
                return self.path

        def localbranches(self):
            branches = []
            bdir = self.join('branches')
            if os.path.isdir(bdir):
                for d in os.listdir(bdir):
                    branches.append(encoding.tolocal(store.decodefilename(d)))
            return branches

        def localbranch(self, name):
            # switch to local branch, creating if necessary
            def checkdir(d):
                if not os.path.isdir(d):
                    if os.path.exists(d):
                        raise util.Abort(_('%s is not a directory') % d)
                    return False
                return True

            if self.dirstate.parents()[1] != nullid:
                raise util.Abort(_('merge in progress'))

            obranch = self.getlocalbranch()
            lname = encoding.fromlocal(name)

            if obranch == name:
                return

            omf = self.changectx('').manifest()
            del self.changelog
            del self.manifest

            if not name:
                lbpath = self.join('localbranch')
                if os.path.exists(lbpath):
                    os.unlink(lbpath)
            else:
                bdir = self.join('branches')
                if not checkdir(bdir):
                    os.mkdir(bdir)
                dest = os.path.join(bdir, store.encodefilename(lname))
                if not checkdir(dest):
                    # check for non-store layout
                    if self.spath == self.path:
                        os.mkdir(dest)
                        datadir = os.path.join(dest, 'data')
                        util.copyfiles(self.join('data'), datadir)
                        for f in ('00changelog.i', '00changelog.d',
                                  '00manifest.i', '00manifest.d'):
                            src = self.join(f)
                            if os.path.exists(src):
                                util.copyfiles(src, os.path.join(dest, f))
                    else:
                        os.mkdir(dest)
                        spath = os.path.join(dest, 'store')
                        util.copyfiles(self.spath, spath)
                self.opener('localbranch', 'w').write(lname + '\n')

            self.loadlocalbranch(name)
            ctx = repo.changectx('tip')
            wlock = self.wlock()
            try:
                self.refreshdirstate(ctx, omf)
            finally:
                del wlock

        def refreshdirstate(self, ctx, omf):
            """Refresh dirstate by invalidating changed entries between
            old manifest omf and new manifest mf"""
            self.dirstate.setparents(ctx.node())
            mf = ctx.manifest()
            omfs = util.set(omf.keys())
            mfs = util.set(mf.keys())
            oldfiles = omfs - mfs
            newfiles = mfs - omfs
            common = omfs & mfs
            for f in oldfiles:
                if self.dirstate[f] != 'a':
                    self.dirstate.forget(f)
            for f in newfiles:
                self.dirstate.normallookup(f)
            for f in common:
                if omf[f] == mf[f]:
                    continue
                if self.dirstate[f] == 'n':
                    self.dirstate.normallookup(f)

        def getlocalbranch(self):
            try:
                branch = repo.opener('localbranch').read().rstrip('\n')
            except IOError:
                branch = ''
            return encoding.tolocal(branch)

        def loadlocalbranch(self, branch):
            spath = self.localbranchpath(encoding.fromlocal(branch))
            if spath != repo.spath:
                if not os.path.isdir(spath):
                    raise util.Abort(_('local branch %s not found') % branch)
                self.store = store.store(self.getrequirements(), spath, util.opener)
                self.spath = self.store.path
                self.sopener = self.store.opener

    repo.__class__ = lbrepo

    if not hasattr(ui, 'repo_path'):
        branch = repo.getlocalbranch()
        if branch:
            repo.loadlocalbranch(branch)

    # UI setup
    ui.repo_path = repo.root

    oldexpandpath = ui.expandpath
    def expandpath(path, default=None):
        ep = oldexpandpath(path, default)
        if ep != path:
            return ep
        bent = store.encodefilename(encoding.fromlocal(path))
        if os.path.isdir(os.path.join('.hg', 'branches', bent)):
            return 'lbranch://%s' % path
        return ep
    ui.expandpath = expandpath

def lbranch(ui, repo, branch=None, delete=False):
    """create or switch to a local branch
    """
    if branch is not None:
        if branch == 'default':
            branch = ''
        if delete:
            if not branch:
                raise util.Abort(_('cannot delete default branch'))
            cur = repo.getlocalbranch()
            if cur == branch:
                repo.localbranch('')
            bpath = repo.localbranchpath(branch)
            if not os.path.isdir(bpath):
                raise util.Abort(_('local branch %s not found') % branch)
            shutil.rmtree(bpath)
            return
        return repo.localbranch(branch)
    elif delete:
        raise util.Abort(_('need a branch name to delete'))

    branch = repo.getlocalbranch() or 'default'
    branches = repo.localbranches()
    branches.sort()
    branches.insert(0, 'default')
    for b in branches:
        if b == branch:
            ui.write('* %s\n' % b)
        else:
            ui.write('%s\n' % b)

cmdtable = {
    "lbranch": (lbranch,
                [('d', 'delete', None, _('delete branch'))],
                _('hg lbranch [BRANCH]'))
    }
