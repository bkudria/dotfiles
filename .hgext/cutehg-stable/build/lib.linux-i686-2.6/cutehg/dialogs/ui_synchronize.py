# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/synchronize.ui'
#
# Created: Wed Jun  3 10:23:39 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_syncDialog(object):
    def setupUi(self, syncDialog):
        syncDialog.setObjectName("syncDialog")
        syncDialog.resize(455, 437)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(syncDialog.sizePolicy().hasHeightForWidth())
        syncDialog.setSizePolicy(sizePolicy)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/vcs_commit.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        syncDialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(syncDialog)
        self.verticalLayout.setSizeConstraint(QtGui.QLayout.SetFixedSize)
        self.verticalLayout.setObjectName("verticalLayout")
        self.urlContainer = QtGui.QWidget(syncDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.urlContainer.sizePolicy().hasHeightForWidth())
        self.urlContainer.setSizePolicy(sizePolicy)
        self.urlContainer.setObjectName("urlContainer")
        self.horizontalLayout_2 = QtGui.QHBoxLayout(self.urlContainer)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.urlLabel = QtGui.QLabel(self.urlContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.urlLabel.sizePolicy().hasHeightForWidth())
        self.urlLabel.setSizePolicy(sizePolicy)
        self.urlLabel.setObjectName("urlLabel")
        self.horizontalLayout_2.addWidget(self.urlLabel)
        self.urlComboBox = QtGui.QComboBox(self.urlContainer)
        self.urlComboBox.setMinimumSize(QtCore.QSize(390, 0))
        self.urlComboBox.setEditable(True)
        self.urlComboBox.setObjectName("urlComboBox")
        self.horizontalLayout_2.addWidget(self.urlComboBox)
        self.verticalLayout.addWidget(self.urlContainer)
        spacerItem = QtGui.QSpacerItem(20, 5, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.verticalLayout.addItem(spacerItem)
        self.line = QtGui.QFrame(syncDialog)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")
        self.verticalLayout.addWidget(self.line)
        self.protocolContainer = QtGui.QWidget(syncDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.protocolContainer.sizePolicy().hasHeightForWidth())
        self.protocolContainer.setSizePolicy(sizePolicy)
        self.protocolContainer.setObjectName("protocolContainer")
        self.horizontalLayout_3 = QtGui.QHBoxLayout(self.protocolContainer)
        self.horizontalLayout_3.setSizeConstraint(QtGui.QLayout.SetDefaultConstraint)
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.protocolLabel = QtGui.QLabel(self.protocolContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.protocolLabel.sizePolicy().hasHeightForWidth())
        self.protocolLabel.setSizePolicy(sizePolicy)
        self.protocolLabel.setObjectName("protocolLabel")
        self.horizontalLayout_3.addWidget(self.protocolLabel)
        self.protocolComboBox = QtGui.QComboBox(self.protocolContainer)
        self.protocolComboBox.setObjectName("protocolComboBox")
        self.protocolComboBox.addItem(QtCore.QString())
        self.protocolComboBox.addItem(QtCore.QString())
        self.protocolComboBox.addItem(QtCore.QString())
        self.protocolComboBox.addItem(QtCore.QString())
        self.horizontalLayout_3.addWidget(self.protocolComboBox)
        self.verticalLayout.addWidget(self.protocolContainer)
        spacerItem1 = QtGui.QSpacerItem(20, 3, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.verticalLayout.addItem(spacerItem1)
        self.optionsStackWidget = QtGui.QStackedWidget(syncDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.optionsStackWidget.sizePolicy().hasHeightForWidth())
        self.optionsStackWidget.setSizePolicy(sizePolicy)
        self.optionsStackWidget.setFrameShape(QtGui.QFrame.Panel)
        self.optionsStackWidget.setFrameShadow(QtGui.QFrame.Sunken)
        self.optionsStackWidget.setObjectName("optionsStackWidget")
        self.folderPage = QtGui.QWidget()
        self.folderPage.setObjectName("folderPage")
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.folderPage)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.folderContainer = QtGui.QWidget(self.folderPage)
        self.folderContainer.setObjectName("folderContainer")
        self.horizontalLayout_4 = QtGui.QHBoxLayout(self.folderContainer)
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.browseLineEdit = QtGui.QLineEdit(self.folderContainer)
        self.browseLineEdit.setObjectName("browseLineEdit")
        self.horizontalLayout_4.addWidget(self.browseLineEdit)
        self.browseButton = QtGui.QPushButton(self.folderContainer)
        self.browseButton.setObjectName("browseButton")
        self.horizontalLayout_4.addWidget(self.browseButton)
        self.verticalLayout_3.addWidget(self.folderContainer)
        self.optionsStackWidget.addWidget(self.folderPage)
        self.remotePage = QtGui.QWidget()
        self.remotePage.setObjectName("remotePage")
        self.verticalLayout_4 = QtGui.QVBoxLayout(self.remotePage)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.hostContainer = QtGui.QWidget(self.remotePage)
        self.hostContainer.setObjectName("hostContainer")
        self.horizontalLayout_5 = QtGui.QHBoxLayout(self.hostContainer)
        self.horizontalLayout_5.setSpacing(2)
        self.horizontalLayout_5.setMargin(2)
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.loginContainer = QtGui.QWidget(self.hostContainer)
        self.loginContainer.setObjectName("loginContainer")
        self.formLayout = QtGui.QFormLayout(self.loginContainer)
        self.formLayout.setFieldGrowthPolicy(QtGui.QFormLayout.AllNonFixedFieldsGrow)
        self.formLayout.setMargin(0)
        self.formLayout.setSpacing(4)
        self.formLayout.setObjectName("formLayout")
        self.hostLabel = QtGui.QLabel(self.loginContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.hostLabel.sizePolicy().hasHeightForWidth())
        self.hostLabel.setSizePolicy(sizePolicy)
        self.hostLabel.setObjectName("hostLabel")
        self.formLayout.setWidget(0, QtGui.QFormLayout.LabelRole, self.hostLabel)
        self.widget = QtGui.QWidget(self.loginContainer)
        self.widget.setObjectName("widget")
        self.horizontalLayout_7 = QtGui.QHBoxLayout(self.widget)
        self.horizontalLayout_7.setSpacing(0)
        self.horizontalLayout_7.setMargin(0)
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.hostLineEdit = QtGui.QLineEdit(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.MinimumExpanding, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.hostLineEdit.sizePolicy().hasHeightForWidth())
        self.hostLineEdit.setSizePolicy(sizePolicy)
        self.hostLineEdit.setMinimumSize(QtCore.QSize(200, 0))
        self.hostLineEdit.setObjectName("hostLineEdit")
        self.horizontalLayout_7.addWidget(self.hostLineEdit)
        spacerItem2 = QtGui.QSpacerItem(5, 5, QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_7.addItem(spacerItem2)
        self.portLabel = QtGui.QLabel(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Maximum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.portLabel.sizePolicy().hasHeightForWidth())
        self.portLabel.setSizePolicy(sizePolicy)
        self.portLabel.setObjectName("portLabel")
        self.horizontalLayout_7.addWidget(self.portLabel)
        self.portLineEdit = QtGui.QLineEdit(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.portLineEdit.sizePolicy().hasHeightForWidth())
        self.portLineEdit.setSizePolicy(sizePolicy)
        self.portLineEdit.setMinimumSize(QtCore.QSize(60, 0))
        self.portLineEdit.setMaximumSize(QtCore.QSize(80, 16777215))
        self.portLineEdit.setObjectName("portLineEdit")
        self.horizontalLayout_7.addWidget(self.portLineEdit)
        self.formLayout.setWidget(0, QtGui.QFormLayout.FieldRole, self.widget)
        self.folderLabel = QtGui.QLabel(self.loginContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.folderLabel.sizePolicy().hasHeightForWidth())
        self.folderLabel.setSizePolicy(sizePolicy)
        self.folderLabel.setObjectName("folderLabel")
        self.formLayout.setWidget(1, QtGui.QFormLayout.LabelRole, self.folderLabel)
        self.folderLineEdit = QtGui.QLineEdit(self.loginContainer)
        self.folderLineEdit.setObjectName("folderLineEdit")
        self.formLayout.setWidget(1, QtGui.QFormLayout.FieldRole, self.folderLineEdit)
        self.userLabel = QtGui.QLabel(self.loginContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.userLabel.sizePolicy().hasHeightForWidth())
        self.userLabel.setSizePolicy(sizePolicy)
        self.userLabel.setObjectName("userLabel")
        self.formLayout.setWidget(2, QtGui.QFormLayout.LabelRole, self.userLabel)
        self.userLineEdit = QtGui.QLineEdit(self.loginContainer)
        self.userLineEdit.setObjectName("userLineEdit")
        self.formLayout.setWidget(2, QtGui.QFormLayout.FieldRole, self.userLineEdit)
        self.passwordLabel = QtGui.QLabel(self.loginContainer)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.passwordLabel.sizePolicy().hasHeightForWidth())
        self.passwordLabel.setSizePolicy(sizePolicy)
        self.passwordLabel.setObjectName("passwordLabel")
        self.formLayout.setWidget(3, QtGui.QFormLayout.LabelRole, self.passwordLabel)
        self.passwordLineEdit = QtGui.QLineEdit(self.loginContainer)
        self.passwordLineEdit.setEchoMode(QtGui.QLineEdit.Password)
        self.passwordLineEdit.setObjectName("passwordLineEdit")
        self.formLayout.setWidget(3, QtGui.QFormLayout.FieldRole, self.passwordLineEdit)
        self.horizontalLayout_5.addWidget(self.loginContainer)
        self.verticalLayout_4.addWidget(self.hostContainer)
        self.optionsStackWidget.addWidget(self.remotePage)
        self.verticalLayout.addWidget(self.optionsStackWidget)
        self.showLog = QtGui.QCheckBox(syncDialog)
        self.showLog.setObjectName("showLog")
        self.verticalLayout.addWidget(self.showLog)
        self.log = QtGui.QListWidget(syncDialog)
        self.log.setEnabled(True)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.log.sizePolicy().hasHeightForWidth())
        self.log.setSizePolicy(sizePolicy)
        self.log.setMinimumSize(QtCore.QSize(0, 125))
        self.log.setMaximumSize(QtCore.QSize(16777215, 125))
        self.log.setAutoScrollMargin(16)
        self.log.setSelectionMode(QtGui.QAbstractItemView.ContiguousSelection)
        self.log.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.log.setProperty("isWrapping", QtCore.QVariant(False))
        self.log.setResizeMode(QtGui.QListView.Fixed)
        self.log.setLayoutMode(QtGui.QListView.SinglePass)
        self.log.setWordWrap(False)
        self.log.setObjectName("log")
        self.verticalLayout.addWidget(self.log)
        self.buttonContainer = QtGui.QWidget(syncDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.buttonContainer.sizePolicy().hasHeightForWidth())
        self.buttonContainer.setSizePolicy(sizePolicy)
        self.buttonContainer.setObjectName("buttonContainer")
        self.horizontalLayout = QtGui.QHBoxLayout(self.buttonContainer)
        self.horizontalLayout.setSpacing(4)
        self.horizontalLayout.setContentsMargins(4, 0, 4, 0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        spacerItem3 = QtGui.QSpacerItem(40, 5, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem3)
        self.cancelButton = QtGui.QPushButton(self.buttonContainer)
        self.cancelButton.setObjectName("cancelButton")
        self.horizontalLayout.addWidget(self.cancelButton)
        spacerItem4 = QtGui.QSpacerItem(15, 5, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem4)
        self.syncButton = QtGui.QPushButton(self.buttonContainer)
        self.syncButton.setObjectName("syncButton")
        self.horizontalLayout.addWidget(self.syncButton)
        self.verticalLayout.addWidget(self.buttonContainer)
        self.actionCancel = QtGui.QAction(syncDialog)
        self.actionCancel.setObjectName("actionCancel")
        self.actionSync = QtGui.QAction(syncDialog)
        self.actionSync.setObjectName("actionSync")

        self.retranslateUi(syncDialog)
        self.optionsStackWidget.setCurrentIndex(1)
        QtCore.QObject.connect(self.syncButton, QtCore.SIGNAL("clicked()"), self.actionSync.trigger)
        QtCore.QObject.connect(self.cancelButton, QtCore.SIGNAL("clicked()"), self.actionCancel.trigger)
        QtCore.QMetaObject.connectSlotsByName(syncDialog)

    def retranslateUi(self, syncDialog):
        syncDialog.setWindowTitle(QtGui.QApplication.translate("syncDialog", "Mercurial Synchronize", None, QtGui.QApplication.UnicodeUTF8))
        self.urlLabel.setText(QtGui.QApplication.translate("syncDialog", "URL:", None, QtGui.QApplication.UnicodeUTF8))
        self.urlComboBox.setToolTip(QtGui.QApplication.translate("syncDialog", "A URL of the location you wish to push, pull, or clone from. \n"
"\n"
"Examples:\n"
"\n"
"http://user@myhost.com/user/myrepo\n"
"\n"
"ssh://user@myhost.com//home/hg/myrepo\n"
"\n"
"", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolLabel.setText(QtGui.QApplication.translate("syncDialog", "Protocol: ", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolComboBox.setToolTip(QtGui.QApplication.translate("syncDialog", "Select the source type.\n"
"\n"
"Local is a location on your computer or a network share you can browse to using a file browser.\n"
"\n"
"HTTP, HTTPS, and SSH are all remote locations using different network protocols.", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolComboBox.setItemText(0, QtGui.QApplication.translate("syncDialog", "Local", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolComboBox.setItemText(1, QtGui.QApplication.translate("syncDialog", "HTTP", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolComboBox.setItemText(2, QtGui.QApplication.translate("syncDialog", "HTTPS", None, QtGui.QApplication.UnicodeUTF8))
        self.protocolComboBox.setItemText(3, QtGui.QApplication.translate("syncDialog", "SSH", None, QtGui.QApplication.UnicodeUTF8))
        self.browseButton.setText(QtGui.QApplication.translate("syncDialog", "Browse...", None, QtGui.QApplication.UnicodeUTF8))
        self.hostLabel.setText(QtGui.QApplication.translate("syncDialog", "Host:", None, QtGui.QApplication.UnicodeUTF8))
        self.hostLineEdit.setToolTip(QtGui.QApplication.translate("syncDialog", "The remote host name or ip address the repository is stored on.", None, QtGui.QApplication.UnicodeUTF8))
        self.portLabel.setText(QtGui.QApplication.translate("syncDialog", "Port:", None, QtGui.QApplication.UnicodeUTF8))
        self.portLineEdit.setToolTip(QtGui.QApplication.translate("syncDialog", "The port number the repository is being served from.", None, QtGui.QApplication.UnicodeUTF8))
        self.folderLabel.setText(QtGui.QApplication.translate("syncDialog", "Folder:", None, QtGui.QApplication.UnicodeUTF8))
        self.folderLineEdit.setToolTip(QtGui.QApplication.translate("syncDialog", "The location on the server the repository is located at.\n"
"\n"
"For SSH the path is a relative path from where the user logging in would normally be.\n"
"Ex\n"
"/repository/blah is really /home/myuser/repository/blah\n"
"\n"
"For the full path use\n"
"//home/hg/repos/myrepo", None, QtGui.QApplication.UnicodeUTF8))
        self.userLabel.setText(QtGui.QApplication.translate("syncDialog", "User:", None, QtGui.QApplication.UnicodeUTF8))
        self.userLineEdit.setToolTip(QtGui.QApplication.translate("syncDialog", "The remote user name.", None, QtGui.QApplication.UnicodeUTF8))
        self.passwordLabel.setText(QtGui.QApplication.translate("syncDialog", "Password:", None, QtGui.QApplication.UnicodeUTF8))
        self.passwordLineEdit.setToolTip(QtGui.QApplication.translate("syncDialog", "The remote user password.", None, QtGui.QApplication.UnicodeUTF8))
        self.showLog.setText(QtGui.QApplication.translate("syncDialog", "Show Log", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setText(QtGui.QApplication.translate("syncDialog", "Close", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setShortcut(QtGui.QApplication.translate("syncDialog", "Esc", None, QtGui.QApplication.UnicodeUTF8))
        self.syncButton.setToolTip(QtGui.QApplication.translate("syncDialog", "Push or Pull changes to or from selected repository to the local one.", None, QtGui.QApplication.UnicodeUTF8))
        self.syncButton.setText(QtGui.QApplication.translate("syncDialog", "Synchronize", None, QtGui.QApplication.UnicodeUTF8))
        self.syncButton.setShortcut(QtGui.QApplication.translate("syncDialog", "Ctrl+Return", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCancel.setText(QtGui.QApplication.translate("syncDialog", "cancel", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCancel.setShortcut(QtGui.QApplication.translate("syncDialog", "Esc", None, QtGui.QApplication.UnicodeUTF8))
        self.actionSync.setText(QtGui.QApplication.translate("syncDialog", "sync", None, QtGui.QApplication.UnicodeUTF8))
        self.actionSync.setShortcut(QtGui.QApplication.translate("syncDialog", "Ctrl+Return", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc