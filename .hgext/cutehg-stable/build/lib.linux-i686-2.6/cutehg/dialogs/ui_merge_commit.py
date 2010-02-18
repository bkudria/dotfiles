# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/merge_commit.ui'
#
# Created: Wed Jun  3 10:23:38 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_mergeCommit(object):
    def setupUi(self, mergeCommit):
        mergeCommit.setObjectName("mergeCommit")
        mergeCommit.resize(552, 471)
        self.verticalLayout = QtGui.QVBoxLayout(mergeCommit)
        self.verticalLayout.setObjectName("verticalLayout")
        self.mergeCommitLabel = QtGui.QLabel(mergeCommit)
        self.mergeCommitLabel.setObjectName("mergeCommitLabel")
        self.verticalLayout.addWidget(self.mergeCommitLabel)
        self.messageTextEdit = QtGui.QTextEdit(mergeCommit)
        self.messageTextEdit.setObjectName("messageTextEdit")
        self.verticalLayout.addWidget(self.messageTextEdit)
        self.line = QtGui.QFrame(mergeCommit)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")
        self.verticalLayout.addWidget(self.line)
        self.statusView = QtGui.QTableView(mergeCommit)
        self.statusView.setObjectName("statusView")
        self.verticalLayout.addWidget(self.statusView)

        self.retranslateUi(mergeCommit)
        QtCore.QMetaObject.connectSlotsByName(mergeCommit)

    def retranslateUi(self, mergeCommit):
        mergeCommit.setWindowTitle(QtGui.QApplication.translate("mergeCommit", "Form", None, QtGui.QApplication.UnicodeUTF8))
        self.mergeCommitLabel.setText(QtGui.QApplication.translate("mergeCommit", "Merge completed. Enter a message for the merge commit.", None, QtGui.QApplication.UnicodeUTF8))

