"""Interactive history editing.

Inspired by git rebase --interactive.
"""
try:
    import cPickle as pickle
except ImportError:
    import pickle
import tempfile
import os

from mercurial import cmdutil
from mercurial import error
from mercurial import hg
from mercurial import node
from mercurial import repair
from mercurial import patch
from mercurial import util
from mercurial import url
from mercurial.i18n import _

# almost entirely stolen from the git-rebase--interactive.sh source
editcomment = """

# Edit history between %s and %s
#
# Commands:
#  p, pick = use commit
#  e, edit = use commit, but stop for amending
#  f, fold = use commit, but fold into previous commit
#  d, drop = remove commit from history
#
"""

def between(repo, old, new):
    revs = [old, ]
    current = old
    while current != new:
        ctx = repo[current]
        if len(ctx.children()) > 1:
            raise util.Abort('cannot edit history that would orphan nodes')
        if len(ctx.parents()) != 1 and ctx.parents()[1] != node.nullid:
            raise util.Abort("can't edit history with merges")
        if not ctx.children():
            current = new
        else:
            current = ctx.children()[0].node()
            revs.append(current)
    if len(repo[current].children()):
        raise util.Abort('cannot edit history that would orphan nodes')
    return revs


def pick(ui, repo, ctx, ha, opts):
    oldctx = repo[ha]
    if oldctx.parents()[0] == ctx:
        ui.debug('node %s unchanged\n' % ha)
        return oldctx, [], [], []
    hg.update(repo, ctx.node())
    fd, patchfile = tempfile.mkstemp(prefix='hg-histedit-')
    fp = os.fdopen(fd, 'w')
    diffopts = patch.diffopts(ui, opts)
    diffopts.git = True
    gen = patch.diff(repo, oldctx.parents()[0].node(), ha, opts=diffopts)
    for chunk in gen:
        fp.write(chunk)
    fp.close()
    try:
        files = {}
        try:
            patch.patch(patchfile, ui, cwd=repo.root, files=files, eolmode=None)
            if not files:
                ui.warn(_('%s: empty changeset')
                             % node.hex(ha))
                return ctx, [], [], []
        finally:
            files = patch.updatedir(ui, repo, files)
            os.unlink(patchfile)
    except Exception, inst:
        raise util.Abort(_('Fix up the change and run '
                           'hg histedit --continue'))
    n = repo.commit(text=oldctx.description(), user=oldctx.user(), date=oldctx.date(),
                    extra=oldctx.extra())
    return repo[n], [n, ], [oldctx.node(), ], []


def edit(ui, repo, ctx, ha, opts):
    oldctx = repo[ha]
    hg.update(repo, ctx.node())
    fd, patchfile = tempfile.mkstemp(prefix='hg-histedit-')
    fp = os.fdopen(fd, 'w')
    diffopts = patch.diffopts(ui, opts)
    diffopts.git = True
    gen = patch.diff(repo, oldctx.parents()[0].node(), ha, opts=diffopts)
    for chunk in gen:
        fp.write(chunk)
    fp.close()
    try:
        files = {}
        try:
            patch.patch(patchfile, ui, cwd=repo.root, files=files, eolmode=None)
        finally:
            files = patch.updatedir(ui, repo, files)
            os.unlink(patchfile)
    except Exception, inst:
        pass
    raise util.Abort('Make changes as needed, you may commit or record as '
                     'needed now.\nWhen you are finished, run hg'
                     ' histedit --continue to resume.')

def fold(ui, repo, ctx, ha, opts):
    oldctx = repo[ha]
    hg.update(repo, ctx.node())
    fd, patchfile = tempfile.mkstemp(prefix='hg-histedit-')
    fp = os.fdopen(fd, 'w')
    diffopts = patch.diffopts(ui, opts)
    diffopts.git = True
    gen = patch.diff(repo, oldctx.parents()[0].node(), ha, opts=diffopts)
    for chunk in gen:
        fp.write(chunk)
    fp.close()
    try:
        files = {}
        try:
            patch.patch(patchfile, ui, cwd=repo.root, files=files, eolmode=None)
            if not files:
                ui.warn(_('%s: empty changeset')
                             % node.hex(ha))
                return ctx, [], [], []
        finally:
            files = patch.updatedir(ui, repo, files)
            os.unlink(patchfile)
    except Exception, inst:
        raise util.Abort(_('Fix up the change and run '
                           'hg histedit --continue'))
    n = repo.commit(text='fold-temp-revision %s' % ha, user=oldctx.user(), date=oldctx.date(),
                    extra=oldctx.extra())
    return finishfold(ui, repo, ctx, oldctx, n, opts, [])

def finishfold(ui, repo, ctx, oldctx, newnode, opts, internalchanges):
    parent = ctx.parents()[0].node()
    hg.update(repo, parent)
    fd, patchfile = tempfile.mkstemp(prefix='hg-histedit-')
    fp = os.fdopen(fd, 'w')
    diffopts = patch.diffopts(ui, opts)
    diffopts.git = True
    gen = patch.diff(repo, parent, newnode, opts=diffopts)
    for chunk in gen:
        fp.write(chunk)
    fp.close()
    files = {}
    try:
        patch.patch(patchfile, ui, cwd=repo.root, files=files, eolmode=None)
    finally:
        files = patch.updatedir(ui, repo, files)
        os.unlink(patchfile)
    newmessage = '\n***\n'.join(
        [ctx.description(), ] +
        [repo[r].description() for r in internalchanges] +
        [oldctx.description(), ])
    newmessage = ui.edit(newmessage, ui.username())
    n = repo.commit(text=newmessage, user=ui.username(), date=max(ctx.date(), oldctx.date()),
                    extra=oldctx.extra())
    return repo[n], [n, ], [oldctx.node(), ctx.node() ], [newnode, ] # xxx

def drop(ui, repo, ctx, ha, opts):
    return ctx, [], [repo[ha].node(), ], []


actiontable = {'p': pick,
               'pick': pick,
               'e': edit,
               'edit': edit,
               'f': fold,
               'fold': fold,
               'd': drop,
               'drop': drop,
               }
def histedit(ui, repo, *parent, **opts):
    """hg histedit <parent>
    """
    if opts.get('outgoing'):
        if len(parent) > 1:
            raise util.Abort('only one repo argument allowed with --outgoing')
        elif parent:
            parent = parent[0]

        dest, revs, checkout = hg.parseurl(
            ui.expandpath(parent or 'default-push', parent or 'default'), ['tip'])
        if revs:
            revs = [repo.lookup(rev) for rev in revs]

        other = hg.repository(ui, dest)
        ui.status(_('comparing with %s\n') % url.hidepassword(dest))
        parent = repo.findoutgoing(other, force=opts.get('force'))
    else:
        if opts.get('force'):
            raise util.Abort('--force only allowed with --outgoing')

    if opts.get('continue', False):
        if len(parent) != 0:
            raise util.Abort('no arguments allowed with --continue')
        (parentctxnode, created, replaced,
         tmpnodes, existing, rules, keep, tip, ) = readstate(repo)
        currentparent, wantnull = repo.dirstate.parents()
        parentctx = repo[parentctxnode]
        # discover any nodes the user has added in the interim
        newchildren = [c for c in parentctx.children()
                       if c.node() not in existing]
        action, currentnode = rules.pop(0)
        while newchildren:
            if action in ['f', 'fold', ]:
                tmpnodes.extend([n.node() for n in newchildren])
            else:
                created.extend([n.node() for n in newchildren])
            newchildren = filter(lambda x: x.node() not in existing,
                                 reduce(lambda x, y: x + y,
                                        map(lambda r: r.children(),
                                            newchildren)))
        m, a, r, d = repo.status()[:4]
        oldctx = repo[currentnode]
        message = oldctx.description()
        if action in ('e', 'edit', ):
            message = ui.edit(message, ui.username())
        elif action in ('f', 'fold', ):
            message = 'fold-temp-revision %s' % currentnode
        new = None
        if m or a or r or d:
            new = repo.commit(text=message, user=oldctx.user(), date=oldctx.date(),
                              extra=oldctx.extra())

        if action in ('e', 'edit', 'p', 'pick', ):
            replaced.append(oldctx.node())
            if new:
                created.append(new)
                parentctx = repo[new]
        else: # fold
            if new:
                tmpnodes.append(new)
            else:
                new = newchildren[-1]
            (parentctx, created_,
             replaced_, tmpnodes_, ) = finishfold(ui, repo,
                                                  parentctx, oldctx, new,
                                                  opts, newchildren)
            replaced.extend(replaced_)
            created.extend(created_)
            tmpnodes.extend(tmpnodes_)

    elif opts.get('abort', False):
        if len(parent) != 0:
            raise util.Abort('no arguments allowed with --abort')
        (parentctxnode, created, replaced, tmpnodes,
         existing, rules, keep, tip, ) = readstate(repo)
        ui.debug('restore wc to old tip %s\n' % node.hex(tip))
        hg.clean(repo, tip)
        ui.debug('should strip created nodes %s\n' %
                 ', '.join([node.hex(n)[:12] for n in created]))
        ui.debug('should strip temp nodes %s\n' %
                 ', '.join([node.hex(n)[:12] for n in tmpnodes]))
        for nodes in (created, tmpnodes, ):
            for n in reversed(nodes):
                try:
                    repair.strip(ui, repo, n)
                except error.LookupError:
                    pass
        os.unlink(os.path.join(repo.path, 'histedit-state'))
        return
    else:
        cmdutil.bail_if_changed(repo)
        if os.path.exists(os.path.join(repo.path, 'histedit-state')):
            raise util.Abort('history edit already in progress, try --continue or --abort')

        tip, empty = repo.dirstate.parents()


        if len(parent) != 1:
            raise util.Abort('requires exactly one parent revision')
        parent = parent[0]

        revs = between(repo, parent, tip)

        ctxs = [repo[r] for r in revs]
        existing = [r.node() for r in ctxs]
        rules = '\n'.join([('pick %s %s' % (c.hex()[:12],
                                         c.description().splitlines()[0]))[:80]
                           for c in ctxs])

        rules += editcomment % (node.hex(parent)[:12], node.hex(tip)[:12], )

        rules = ui.edit(rules, ui.username())

        parentctx = repo[parent].parents()[0]

        rules = [l for l in (r.strip() for r in rules.splitlines()) if l and not l[0] == '#']
        rules = verifyrules(rules, repo, ctxs)
        keep = opts.get('keep', False)
        replaced = []
        tmpnodes = []
        created = []


    while rules:
        writestate(repo, parentctx.node(), created, replaced, tmpnodes, existing,
                   rules, keep, tip)
        action, ha = rules.pop(0)
        (parentctx, created_,
         replaced_, tmpnodes_, ) = actiontable[action](ui, repo,
                                                       parentctx, ha,
                                                       opts)
        created.extend(created_)
        replaced.extend(replaced_)
        tmpnodes.extend(tmpnodes_)

    hg.update(repo, parentctx.node())

    if not keep:
        ui.debug('should strip replaced nodes %s\n' %
                 ', '.join([node.hex(n)[:12] for n in replaced]))
        for n in sorted(replaced, lambda x, y: cmp(repo[x].rev(), repo[y].rev())):
            try:
                repair.strip(ui, repo, n)
            except error.LookupError:
                pass

    ui.debug('should strip temp nodes %s\n' %
             ', '.join([node.hex(n)[:12] for n in tmpnodes]))
    for n in reversed(tmpnodes):
        try:
            repair.strip(ui, repo, n)
        except error.LookupError:
            pass
    os.unlink(os.path.join(repo.path, 'histedit-state'))


def writestate(repo, parentctxnode, created, replaced,
               tmpnodes, existing, rules, keep, oldtip):
    fp = open(os.path.join(repo.path, 'histedit-state'), 'w')
    pickle.dump((parentctxnode, created, replaced,
                 tmpnodes, existing, rules, keep, oldtip,),
                fp)
    fp.close()

def readstate(repo):
    """Returns a tuple of (parentnode, created, replaced, tmp, existing, rules, keep, oldtip, ).
    """
    fp = open(os.path.join(repo.path, 'histedit-state'))
    return pickle.load(fp)


def verifyrules(rules, repo, ctxs):
    """Verify that there exists exactly one edit rule per given changeset.

    Will abort if there are to many or too few rules, a malformed rule,
    or a rule on a changeset outside of the user-given range.
    """
    parsed = []
    first = True
    if len(rules) != len(ctxs):
        raise util.Abort('must specify a rule for each changeset once')
    for r in rules:
        if ' ' not in r:
            raise util.Abort('malformed line "%s"' % r)
        action, rest = r.split(' ', 1)
        if ' ' in rest.strip():
            ha, rest = rest.split(' ', 1)
        else:
            ha = r.strip()
        try:
            if repo[ha] not in ctxs:
                raise util.Abort('may not use changesets other than the ones listed')
        except error.RepoError:
            raise util.Abort('unknown changeset %s listed' % ha)
        if action not in actiontable:
            raise util.Abort('unknown action "%s"' % action)
        parsed.append([action, ha])
    return parsed


cmdtable = {
    "histedit":
        (histedit,
         [('c', 'continue', False, 'continue an edit already in progress', ),
          ('k', 'keep', False, 'strip old nodes after edit is complete', ),
          ('', 'abort', False, 'abort an edit in progress', ),
          ('o', 'outgoing', False, 'changesets not found in destination'),
          ('f', 'force', False, 'force outgoing even for unrelated repositories'),
          ],
         __doc__,
         ),
}
