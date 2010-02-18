#!/usr/bin/env python

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

from PyQt4 import QtCore, QtGui

def createTagPolygon(font, text):
    ''' create an empty polygon that will fit the text 
    with the given font using the tag shape and color
    '''

    fontMetrics = QtGui.QFontMetrics(font)
    textRect = fontMetrics.boundingRect(text)

    width = textRect.width() + 6
    height = textRect.height() + 6

    polygon = QtGui.QPolygon()
    points = [ 4, 0, 
              0, height/2, 
              4, height,
              width + 4, height,
              width,     height/2,
              width + 4, 0
            ]
    polygon.setPoints(points)

    return polygon


class HistoryDecorator(object):
    def __init__(self):
        self.COLUMN_WIDTH = 9 # number of pixels each column takes up

    def paint(self, item, pitem):
        ''' return a QImage with the graph drawing in it
        '''

        # determine the correct size for the QImage
        tags = item.tags
        font = QtGui.QApplication.font()
        width = 0
        height = 0
        for tag in tags:
            rect = createTagPolygon(font, QtCore.QString(tag)).boundingRect()
            width = width + rect.width() + 10
            height = max(height, rect.height() + 10)


        size = QtCore.QSize(len(item.edges)*self.COLUMN_WIDTH + 9, 15)
      
        image = QtGui.QImage(size, QtGui.QImage.Format_ARGB32)
        painter = QtGui.QPainter(image)

        VALUE = 0.7
        SATURATION = 1.0

        font = QtGui.QApplication.font()
        palette = QtGui.QApplication.palette()

        # setup painter
        painter.setRenderHint(QtGui.QPainter.Antialiasing, True)
        pedges = None
        if pitem is None:
            pedges = []
        else:
            pedges = pitem.edges
        

        currentX = 3
        currentY = 3

        # draw each incoming line drawing downwards from the center of the previous cell 
        for (col, next_col, edge_color) in pedges:
            beginY = size.height()/2
            endY = size.height()/2 
            beginX = col*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2
            endX = beginX 
            
            if next_col != col:
                endX = next_col*self.COLUMN_WIDTH  + self.COLUMN_WIDTH/2

            qcolor = QtGui.QColor.fromHsvF( self._colorRandomizer(edge_color), SATURATION, VALUE)
            painter.setPen(qcolor)
            painter.setBrush(qcolor)
            path = QtGui.QPainterPath()
            path.moveTo(beginX, beginY)
            path.cubicTo(QtCore.QPointF(beginX, beginY + (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY - (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY))
            painter.strokePath( path, painter.pen() )
   
        # draw each outgoing line drawing downwards from the middle of the cell to the middle of the next cell (one below)
        for (col, next_col, edge_color) in item.edges:
            beginY =  size.height()/2 + 1
            endY = size.height() + size.height()/2
            beginX = col*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2
            endX = beginX 
            
            if next_col != col:
                endX = next_col*self.COLUMN_WIDTH  + self.COLUMN_WIDTH/2

            qcolor = QtGui.QColor.fromHsvF( self._colorRandomizer(edge_color), SATURATION, VALUE)
            painter.setPen(qcolor)
            painter.setBrush(qcolor)
            path = QtGui.QPainterPath()
            path.moveTo(beginX, beginY)
            path.cubicTo(QtCore.QPointF(beginX, beginY + (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY - (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY))
            painter.strokePath( path, painter.pen() )

        # Draw the node for the cell
        qcolor = QtGui.QColor.fromHsvF( self._colorRandomizer(item.color), SATURATION, VALUE)
        painter.setPen(qcolor)
        painter.setBrush(qcolor)
        painter.drawEllipse( QtCore.QPointF( item.column*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2,
                                             size.height()/2 ),
                             3, 3 )

        painter.setFont(font)
     
        currentX = currentX + len(item.edges)*self.COLUMN_WIDTH + 3
        for tag in item.tags:
            painter.setPen(palette.color(QtGui.QPalette.Dark))
            painter.setBrush(palette.color(QtGui.QPalette.Light))
           
            polygon = createTagPolygon(font, QtCore.QString(tag))
            currentY = (size.height() - polygon.boundingRect().height())/2
            polygon.translate(currentX, currentY)
            painter.drawPolygon(polygon)

            color = palette.color(QtGui.QPalette.WindowText)
            painter.setPen(color)
            painter.setBrush(color)
            rect = polygon.boundingRect()
            rect.translate(-2, 0)
            painter.drawText( rect, QtCore.Qt.AlignCenter, tag )
            
            currentX = currentX + 5 + polygon.boundingRect().width()

        return image

    def _colorRandomizer(self, colorNumber):
        MAX_COLORS = 5 
        number =  (colorNumber % MAX_COLORS) + 1
        # now determine the hue value to return
        return number / float(MAX_COLORS+1) 
            
        
