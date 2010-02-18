#!/usr/bin/env python

# CuteHg - Qt4 Dialog Extension of Mercurial
# Copyright (C) 2009 Tom Burdick 
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

""" A set of dialogs using qt4 for certain mercurial commands
"""
from mercurial import demandimport, util, cmdutil
demandimport.ignore.append('icons_rc')

import sys, os
from mercurial.i18n import _
from mercurial.commands import walkopts

try:
    from PyQt4 import QtCore, QtGui
except ImportError, ex:
    if str(ex) == 'No module named PyQt4':
        sitepath = os.path.dirname(os.path.dirname(__file__))
        libpath = os.path.dirname(sitepath)
        sys.path.extend([sitepath, libpath])
        from PyQt4 import QtCore, QtGui

def cute_update(ui, repo, **opts):
    """ 
    Show the CuteHg Update Dialog
    """
    from update import UpdateDialog
    app = QtGui.QApplication( sys.argv )
    dialog = UpdateDialog(ui, repo)
    dialog.show()
    app.exec_()


def cute_history(ui, repo, **opts):
    """ 
    Show the CuteHg History dialog
    """
    from history import HistoryDialog
    app = QtGui.QApplication( sys.argv )
    dialog = HistoryDialog(ui, repo)
    dialog.show()
    app.exec_()


def cute_commit(ui, repo, **opts):
    """
    Show the CuteHg commit dialog
    """
    app = QtGui.QApplication( sys.argv )
    #dialog = CommitDialog(ui, repo, **opts)
    sys.exit(app.exec_())

def cute_pull(ui, repo, **opts):
    """
    Show the CuteHg Pull dialog
    """
    from pull import PullDialog
    app = QtGui.QApplication( sys.argv )
    dialog = PullDialog(ui, repo)
    dialog.show()
    sys.exit(app.exec_())

def cute_push(ui, repo, **opts):
    """
    Show the CuteHg Push dialog
    """
    from push import PushDialog
    app = QtGui.QApplication( sys.argv )
    dialog = PushDialog(ui, repo)
    dialog.show()
    sys.exit(app.exec_())


def cute_clone(ui, repo, **opts):
    """
    Show the CuteHg Clone dialog
    """
    app = QtGui.QApplication( sys.argv )
    #dialog = CloneDialog(ui, repo, **opts)
    sys.exit(app.exec_())

def cute_annotate(ui, repo, *pats, **opts):
    """
    Show the CuteHg Annotate dialog

    show changeset information per file line

    List changes in files, showing the revision id responsible for each line

    This command is useful to discover who did a change or when a change took
    place.

    Without the -a option, annotate will avoid processing files it
    detects as binary. With -a, annotate will generate an annotation
    anyway, probably with undesirable results.
    """
    from annotate import AnnotateDialog

    if not pats:
        raise util.Abort(_('at least one file name or pattern required'))

    if (not opts.get('user') and not opts.get('changeset') and not opts.get('date')
        and not opts.get('follow')):
        opts['number'] = 1

    ctx = repo[opts.get('rev')]

    m = cmdutil.match(repo, pats, opts)
    app = QtGui.QApplication( sys.argv )
    dialogs = []
    for abs in ctx.walk(m):
        fctx = ctx[abs]
        if not opts.get('text') and util.binary(fctx.data()):
            ui.write(_("%s: binary file\n") % ((pats and m.rel(abs)) or abs))
            continue

        dialog = AnnotateDialog(ui, repo, opts.get('rev'), abs)
        dialog.show()
        dialogs.append(dialog)

    sys.exit(app.exec_())


def cute_status(ui, repo, **opts):
    """
    Show the CuteHg Status dialog
    """
    from status import StatusDialog
    app = QtGui.QApplication( sys.argv )
    dialog = StatusDialog(ui, repo)
    dialog.show()
    sys.exit(app.exec_())



cmdtable = {
        "cuteupdate|cup":   
            (cute_update,
            [('', 'update', None, _('open cutehg update dialog'))],
            _('hg cuteupdate') ),
        "cutepull|cull":   
            (cute_pull,
            [('', 'pull', None, _('open cutehg pull dialog'))],
            _('hg cutepull') ),
        "cutepush|cush":   
            (cute_push,
            [('', 'push', None, _('open cutehg push dialog'))],
            _('hg cutepush') ),
        "cutehistory|chist":   
            (cute_history,
            [('', 'history', None, _('open cutehg history dialog'))],
            _('hg cutehistory') ),
        "cuteannotate|can":
            (cute_annotate,
            [('r', 'rev', '', _('annotate the specified revision')),
             ('f', 'follow', None, _('follow file copies and renames')),
             ('a', 'text', None, _('treat all files as text')),
             ] + walkopts,
            _('[-r REV] [-f] [-a] FILE...')),
        "cutestatus|cstat":   
            (cute_status,
            [('', 'status', None, _('open cutehg status dialog'))],
            _('hg cutestatus') ),
        }

