# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/annotate.ui'
#
# Created: Wed Jun  3 10:23:38 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_AnnotateDialog(object):
    def setupUi(self, AnnotateDialog):
        AnnotateDialog.setObjectName("AnnotateDialog")
        AnnotateDialog.resize(800, 600)
        self.centralwidget = QtGui.QWidget(AnnotateDialog)
        self.centralwidget.setObjectName("centralwidget")
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName("verticalLayout")
        AnnotateDialog.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(AnnotateDialog)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 22))
        self.menubar.setObjectName("menubar")
        AnnotateDialog.setMenuBar(self.menubar)
        self.hgActions = QtGui.QToolBar(AnnotateDialog)
        self.hgActions.setMovable(False)
        self.hgActions.setToolButtonStyle(QtCore.Qt.ToolButtonTextUnderIcon)
        self.hgActions.setFloatable(False)
        self.hgActions.setObjectName("hgActions")
        AnnotateDialog.addToolBar(QtCore.Qt.TopToolBarArea, self.hgActions)
        self.actionGotoParent = QtGui.QAction(AnnotateDialog)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/svn_switch.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionGotoParent.setIcon(icon)
        self.actionGotoParent.setObjectName("actionGotoParent")
        self.actionBack = QtGui.QAction(AnnotateDialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/edit-undo.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionBack.setIcon(icon1)
        self.actionBack.setObjectName("actionBack")
        self.hgActions.addAction(self.actionBack)
        self.hgActions.addSeparator()
        self.hgActions.addAction(self.actionGotoParent)
        self.hgActions.addSeparator()

        self.retranslateUi(AnnotateDialog)
        QtCore.QMetaObject.connectSlotsByName(AnnotateDialog)

    def retranslateUi(self, AnnotateDialog):
        AnnotateDialog.setWindowTitle(QtGui.QApplication.translate("AnnotateDialog", "Annotate", None, QtGui.QApplication.UnicodeUTF8))
        self.hgActions.setWindowTitle(QtGui.QApplication.translate("AnnotateDialog", "toolBar", None, QtGui.QApplication.UnicodeUTF8))
        self.actionGotoParent.setText(QtGui.QApplication.translate("AnnotateDialog", "Annotate parent", None, QtGui.QApplication.UnicodeUTF8))
        self.actionBack.setText(QtGui.QApplication.translate("AnnotateDialog", "Back", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
