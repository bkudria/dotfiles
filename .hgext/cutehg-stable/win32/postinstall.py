#!/usr/bin/env python

# CuteHg - Qt4 Dialog Extension of Mercurial
# Copyright (C) 2009 Stefan Rusek 
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

import sys, os, subprocess, ctypes

action = []
if len(sys.argv) > 1:
    action.append('/s')
if len(sys.argv) >= 2 and sys.argv[1] == '-remove':
    action.append('/u')

cmds = [
        ('%WINDIR%\\System32\\regsvr32.exe', 'cutehg-32.dll')
        ('%WINDIR%\\System32\\msiexec.exe', '/i', 'tortoiseoverlays-win32.msi', '/qn', '/norestart')
       ]
if os.path.exists(os.path.expandvars('%WINDIR%\\SysWOW64')):
    cmds.append(('%WINDIR%\\SysWOW64\\regsvr32.exe', 'cutehg-64.dll'))
    cmds.append(('%WINDIR%\\SysWOW64\\msiexec.exe', '/i', 'tortoiseoverlays-x64.msi', '/qn', '/norestart'))

for cmd, file in cmds:
    args = [os.path.expandvars(cmd)] + action + [file]
    proc = subprocess.Popen(args)
    proc.wait()
