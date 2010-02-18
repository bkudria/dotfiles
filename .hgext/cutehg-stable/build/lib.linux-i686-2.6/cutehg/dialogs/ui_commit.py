# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/commit.ui'
#
# Created: Wed Jun  3 10:23:38 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_CommitDialog(object):
    def setupUi(self, CommitDialog):
        CommitDialog.setObjectName("CommitDialog")
        CommitDialog.resize(692, 573)
        self.centralwidget = QtGui.QWidget(CommitDialog)
        self.centralwidget.setObjectName("centralwidget")
        CommitDialog.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(CommitDialog)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 692, 22))
        self.menubar.setObjectName("menubar")
        CommitDialog.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(CommitDialog)
        self.statusbar.setObjectName("statusbar")
        CommitDialog.setStatusBar(self.statusbar)
        self.toolBar = QtGui.QToolBar(CommitDialog)
        self.toolBar.setToolButtonStyle(QtCore.Qt.ToolButtonTextUnderIcon)
        self.toolBar.setObjectName("toolBar")
        CommitDialog.addToolBar(QtCore.Qt.TopToolBarArea, self.toolBar)
        self.actionCommit = QtGui.QAction(CommitDialog)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/im-status-message-edit.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCommit.setIcon(icon)
        self.actionCommit.setObjectName("actionCommit")
        self.actionUpdatePatch = QtGui.QAction(CommitDialog)
        self.actionUpdatePatch.setIcon(icon)
        self.actionUpdatePatch.setObjectName("actionUpdatePatch")
        self.actionCreate_Patch = QtGui.QAction(CommitDialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/svn_add.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCreate_Patch.setIcon(icon1)
        self.actionCreate_Patch.setObjectName("actionCreate_Patch")
        self.actionCancel = QtGui.QAction(CommitDialog)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/dialog-cancel.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCancel.setIcon(icon2)
        self.actionCancel.setObjectName("actionCancel")
        self.actionHelp = QtGui.QAction(CommitDialog)
        self.actionHelp.setObjectName("actionHelp")
        self.actionRefresh = QtGui.QAction(CommitDialog)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(":/view-refresh.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionRefresh.setIcon(icon3)
        self.actionRefresh.setObjectName("actionRefresh")
        self.toolBar.addAction(self.actionRefresh)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionCommit)
        self.toolBar.addAction(self.actionUpdatePatch)
        self.toolBar.addAction(self.actionCreate_Patch)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionCancel)

        self.retranslateUi(CommitDialog)
        QtCore.QMetaObject.connectSlotsByName(CommitDialog)

    def retranslateUi(self, CommitDialog):
        CommitDialog.setWindowTitle(QtGui.QApplication.translate("CommitDialog", "Commit", None, QtGui.QApplication.UnicodeUTF8))
        self.toolBar.setWindowTitle(QtGui.QApplication.translate("CommitDialog", "toolBar", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCommit.setText(QtGui.QApplication.translate("CommitDialog", "Commit", None, QtGui.QApplication.UnicodeUTF8))
        self.actionUpdatePatch.setText(QtGui.QApplication.translate("CommitDialog", "UpdatePatch", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCreate_Patch.setText(QtGui.QApplication.translate("CommitDialog", "Create Patch", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCancel.setText(QtGui.QApplication.translate("CommitDialog", "Cancel", None, QtGui.QApplication.UnicodeUTF8))
        self.actionHelp.setText(QtGui.QApplication.translate("CommitDialog", "?", None, QtGui.QApplication.UnicodeUTF8))
        self.actionRefresh.setText(QtGui.QApplication.translate("CommitDialog", "Refresh", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
