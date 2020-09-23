VERSION 5.00
Begin VB.Form Form1 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   6195
   ClientLeft      =   105
   ClientTop       =   135
   ClientWidth     =   5205
   DrawStyle       =   5  'Transparent
   Icon            =   "BWheel.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   ScaleHeight     =   413
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   347
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CheckBox chkSound 
      BackColor       =   &H00E0E0E0&
      Caption         =   "S"
      Height          =   255
      Left            =   450
      Style           =   1  'Graphical
      TabIndex        =   22
      Top             =   4650
      Width           =   285
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00E0E0E0&
      BorderStyle     =   0  'None
      Caption         =   "Frame1"
      Height          =   375
      Left            =   375
      TabIndex        =   12
      Top             =   5265
      Width           =   4440
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "8"
         Height          =   210
         Index           =   7
         Left            =   3810
         TabIndex        =   21
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "7"
         Height          =   210
         Index           =   6
         Left            =   3345
         TabIndex        =   20
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "6"
         Height          =   210
         Index           =   5
         Left            =   2910
         TabIndex        =   19
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "5"
         Height          =   210
         Index           =   4
         Left            =   2475
         TabIndex        =   18
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "4"
         Height          =   210
         Index           =   3
         Left            =   2070
         TabIndex        =   17
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "3"
         Height          =   210
         Index           =   2
         Left            =   1650
         TabIndex        =   16
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "2"
         Height          =   210
         Index           =   1
         Left            =   1230
         TabIndex        =   15
         Top             =   105
         Width           =   390
      End
      Begin VB.OptionButton optShapes 
         BackColor       =   &H00E0E0E0&
         Caption         =   "1"
         Height          =   210
         Index           =   0
         Left            =   810
         TabIndex        =   14
         Top             =   105
         Width           =   390
      End
      Begin VB.Label Label1 
         BackColor       =   &H00E0E0E0&
         Caption         =   "Shapes"
         Height          =   225
         Left            =   90
         TabIndex        =   13
         Top             =   90
         Width           =   570
      End
   End
   Begin VB.OptionButton optVBASM 
      BackColor       =   &H00E0E0E0&
      Caption         =   "ASM"
      Height          =   195
      Index           =   1
      Left            =   1155
      TabIndex        =   10
      Top             =   4905
      Width           =   660
   End
   Begin VB.OptionButton optVBASM 
      BackColor       =   &H00E0E0E0&
      Caption         =   "VB"
      Height          =   240
      Index           =   0
      Left            =   1155
      TabIndex        =   9
      Top             =   4560
      Width           =   660
   End
   Begin VB.Frame frmROTAAROT 
      BackColor       =   &H00E0E0E0&
      BorderStyle     =   0  'None
      Height          =   570
      Left            =   3360
      TabIndex        =   4
      Top             =   4560
      Width           =   870
      Begin VB.OptionButton optROTAAROT 
         BackColor       =   &H00E0E0E0&
         Caption         =   "AARot"
         Height          =   195
         Index           =   1
         Left            =   45
         TabIndex        =   8
         Top             =   315
         Width           =   840
      End
      Begin VB.OptionButton optROTAAROT 
         BackColor       =   &H00E0E0E0&
         Caption         =   "Rot"
         Height          =   195
         Index           =   0
         Left            =   45
         TabIndex        =   7
         Top             =   30
         Width           =   630
      End
   End
   Begin VB.TextBox txtRot 
      Alignment       =   2  'Center
      BackColor       =   &H00E0E0E0&
      BorderStyle     =   0  'None
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   2325
      TabIndex        =   3
      Text            =   "0"
      Top             =   4440
      Width           =   525
   End
   Begin VB.CheckBox chkExit 
      BackColor       =   &H00E0E0E0&
      Caption         =   "X"
      Height          =   225
      Left            =   4560
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   4665
      Width           =   240
   End
   Begin VB.HScrollBar SBRotSpeed 
      Height          =   240
      LargeChange     =   10
      Left            =   2145
      Max             =   180
      Min             =   -180
      TabIndex        =   1
      Top             =   4665
      Width           =   915
   End
   Begin VB.PictureBox PIC 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   3840
      Left            =   660
      ScaleHeight     =   256
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   0
      Top             =   405
      Width           =   3840
   End
   Begin VB.Label Label3 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Rotation deg"
      BeginProperty Font 
         Name            =   "MS Serif"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   180
      Left            =   2235
      TabIndex        =   11
      Top             =   5010
      Width           =   900
   End
   Begin VB.Shape Shape3 
      Height          =   3900
      Left            =   555
      Shape           =   3  'Circle
      Top             =   405
      Width           =   4095
   End
   Begin VB.Shape Shape2 
      Height          =   5880
      Left            =   60
      Shape           =   4  'Rounded Rectangle
      Top             =   30
      Width           =   5100
   End
   Begin VB.Shape Shape1 
      Height          =   825
      Left            =   1035
      Shape           =   4  'Rounded Rectangle
      Top             =   4410
      Width           =   3270
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00E0E0E0&
      Caption         =   """Spinning illusions""  by  Robert Rayment"
      BeginProperty Font 
         Name            =   "ScriptS"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF8080&
      Height          =   225
      Left            =   525
      TabIndex        =   6
      Top             =   5655
      Width           =   4245
   End
   Begin VB.Label LabTitle 
      Alignment       =   2  'Center
      BackColor       =   &H00E0E0E0&
      Caption         =   "Benham's Wheel Illusion"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   -1  'True
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      Height          =   270
      Left            =   840
      TabIndex        =   5
      Top             =   120
      Width           =   3510
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Spinning illusions  by  Robert Rayment,  Apr 2002


' Benham's Wheel shows colored bands appearing at
' different speeds.   Unless your color blind!

' Ref: http://mathworld.wolfram.com/BenhamsWheel.html  (Mitch Foral, PSC)

' Rotoreliefs, Muval Duchamp (1935)
' ref: www.babot.co.jp/shiro/sensor/rotorelief/roto.htm

' Shaping APIs - from various places on PSC

' Some designs drawn with VB others loaded from 256x256 bitmaps

' Easy to do your own if you want to. See eg DrawLogSpiral & OptShapes.

Option Base 1  ' Array base subscript = 1 not 0
DefLng A-W     ' All variables Long
DefSng X-Z     ' unless singles
               ' unless otherwise specified

Dim FormTop, FormLeft   ' For moving headerless form

Dim Done As Boolean  ' Loop breaker

Dim redc As Byte, greenc As Byte, bluec As Byte  ' Byte RGB colors

Dim ixc, iyc ' PIC centers

Dim PathSpec$  ' App Path



Private Sub Form_Load()

' Get app path
PathSpec$ = App.Path
If Right$(PathSpec$, 1) <> "\" Then PathSpec$ = PathSpec$ & "\"

' Load bin machine code
Loadmcode PathSpec$ & "AARotate.bin", AARotateMC()

ShapeForm

ShapePIC

optShapes(0).Value = True
DrawBenhamWheel

' Initialise to VB & ordinary rotation
optVBASM(0).Value = True         ' VB
VBASM = True
optROTAAROT(0).Value = True      ' Rot
RotAARot = True

chkSound.Value = Unchecked
ASound = False

Show
DoEvents

GETDIBS PIC.Image

' Fill MCode structure
MCODE.PICW = PICW
MCODE.PICH = PICH
MCODE.PtrPalBGR = VarPtr(PalBGR(1, 1, 1, 1))
MCODE.PalSize = 4 * PICW * PICH
ptMC = VarPtr(AARotateMC(1))
ptrStruc = VarPtr(MCODE.PICW)

' Staring angle
txtRot.Text = "0"
zang = 0

' Get wav data
WAVData = StrConv(LoadResData(101, "CUSTOM"), vbUnicode)


ACTIONLOOP


End Sub


Private Sub ACTIONLOOP()

Done = False

Do

   If ASound And Click = 1 Then
      sndPlaySound WAVData, SND_ASYNC Or SND_NODEFAULT Or SND_MEMORY
   End If

   If RotAARot Then RotFlag = 0 Else RotFlag = 1
   
   If VBASM Then
      
      ' VB ROTATE
      VBRotate zang, RotFlag
   
   Else
   
      MCODE.zang = zang

      ' MACHINE CODE ROTATE
      res = CallWindowProc(ptMC, ptrStruc, 0&, 0&, RotFlag)
   
   End If
   
   ' Blit PalBGR()
   If StretchDIBits(PIC.HDC, _
      0, 0, PICW, PICH, _
      0, 0, PICW, PICH, _
      PalBGR(1, 1, 1, 2), bm, _
      1, vbSrcCopy) = 0 Then
         
       MsgBox ("Blit Error")
       Done = True
       Erase PalBGR
       Unload Me
       End
   
   End If

   PIC.Refresh
   
   DoEvents
   
   ' Bump angle
   'ndeg = Val(txtRot.Text)
   zang = zang + Val(txtRot.Text) * d2r#
   If zang < -2 * pi# Then
      zang = zang + 2 * pi#: Click = 1
   ElseIf zang > 2 * pi# Then
      zang = zang - 2 * pi#: Click = 1
   Else
      Click = 0
   End If
   

Loop Until Done

sndPlaySound "", SND_PURGE

End Sub

Private Sub ShapeForm()

Width = 5200
Height = 6000

RoundReg = CreateRoundRectRgn _
(0, 0, Me.Width / Screen.TwipsPerPixelX, Me.Height / Screen.TwipsPerPixelY, 90, 90)

SetWindowRgn Me.hwnd, RoundReg, False

DeleteObject RoundReg

End Sub

Private Sub ShapePIC()

With PIC
   .ScaleMode = vbPixels
   .Height = 256
   .Width = 256
   .Left = (Form1.Width / Screen.TwipsPerPixelX - .Width) \ 2
   .Top = Form1.Top / Screen.TwipsPerPixelY + 20
   .BackColor = RGB(255, 255, 255)
   .AutoRedraw = True
   
   PICH = .Height
   PICW = .Width
End With

' PIC centers
ixc = PIC.Width \ 2
iyc = PIC.Height \ 2

RoundPic = CreateEllipticRgn _
(0, 0, PIC.Width, PIC.Height)

SetWindowRgn PIC.hwnd, RoundPic, False

DeleteObject RoundPic

End Sub

Private Sub DrawBenhamWheel()

LabTitle.Caption = "Benham's Wheel Illusion"
DoEvents

' object.Circle (x, y), radius, [color, start, end, aspect]

PIC.DrawWidth = 2

PIC.FillColor = RGB(255, 255, 255)
PIC.FillStyle = vbTransparent
PIC.Circle (ixc, iyc), 110    ' NB can't do Circle with With

PIC.FillColor = 0
PIC.FillStyle = vbSolid
PIC.Circle (ixc, iyc), 110, 0, -pi#, -0.001

PIC.FillColor = RGB(255, 255, 255)
PIC.FillStyle = vbTransparent
PIC.Circle (ixc, iyc), 10

radreduc = 16
For zAngle = 0 To pi# - pi# / 4 + 0.01 Step pi# / 4

   For radius = 35 To 85 Step 25

      PIC.Circle (ixc, iyc), radius - radreduc, 0, zAngle, zAngle + pi# / 4
   
   Next radius
   radreduc = radreduc - 4
Next zAngle

End Sub

Private Sub DrawLogSpiral(Index As Integer, LineWidth, zA As Single)

If Index = 1 Then
   LabTitle.Caption = "Sparse Log spiral"
   zstep = 0.2
Else
   LabTitle.Caption = "Blob Log spiral"
   zstep = 0.1
End If
DoEvents

'Log spiral R = A x zang
'x = xc + zA zang cos(zang)
'y = yc + zA zang sin(zang)

'Select zA & zang limit to stay in bounds

PIC.DrawWidth = LineWidth
'zA = 2 '0.6
cul = 1
For zang = 0 To 70 * pi# Step zstep
   ix = ixc + zA * zang * Cos(zang)
   iy = iyc + zA * zang * Sin(zang)
   If ix >= 1 And iy >= 1 And ix <= PICW And iy <= PICH Then
      PIC.PSet (ix, iy), 0
   End If
   If cul = 255 Then cul = 0
   cul = cul + 1
Next zang

PIC.DrawWidth = 1

End Sub

Private Sub DrawColorWheel()

LabTitle.Caption = "RGB Color Wheel"
DoEvents

' object.Circle (x, y), radius, [color, start, end, aspect]

PIC.FillStyle = vbSolid
PIC.FillColor = RGB(255, 0, 0)
PIC.Circle (ixc, iyc), 110, 0, -4 * pi# / 3, -0.001

PIC.FillColor = RGB(0, 255, 0)
PIC.Circle (ixc, iyc), 110, 0, -2 * pi# / 3, -4 * pi# / 3

PIC.FillColor = RGB(0, 0, 255)
PIC.Circle (ixc, iyc), 110, 0, -0.001, -2 * pi# / 3

End Sub

Private Sub GETDIBS(ByVal PICIM As Long)

' PICIM is PIC.Image - handle to picbox memory
' from which pixels will be extracted and
' stored in PalBGRA()

On Error GoTo DIBError

'Get info on picture loaded into PIC
GetObjectAPI PICIM, Len(bmp), bmp

NewDC = CreateCompatibleDC(0&)
OldH = SelectObject(NewDC, PICIM)

' Set up bm struc for GetDIBits & StretchDIBits
With bm.bmiH
   .biSize = 40
   .biwidth = bmp.bmWidth
   .biheight = bmp.bmHeight
   .biPlanes = 1
   .biBitCount = 32          ' Sets up BGRA pixels
   .biCompression = 0
   BytesPerScanLine = ((((.biwidth * .biBitCount) + 31) \ 32) * 4)
   PadBytesPerScanLine = _
       BytesPerScanLine - (((.biwidth * .biBitCount) + 7) \ 8)
   .biSizeImage = BytesPerScanLine * Abs(.biheight)
End With

' Not sure about necessity of  31) \ 32)  &  7) \ 8)
   
' Set PalBGR to receive color bytes BGRA
ReDim PalBGR(4, PICW, PICH, 2) As Byte

' Whiten dest PalBGR since top & left edges
' will be left at a color = 0 ie black
For iy = 1 To PICH
For ix = 1 To PICW
   PalBGR(1, ix, iy, 2) = 255
   PalBGR(2, ix, iy, 2) = 255
   PalBGR(3, ix, iy, 2) = 255
Next ix
Next iy

' Load color bytes to 1st half of PalBGR
ret = GetDIBits(NewDC, PICIM, 0, PICH, PalBGR(1, 1, 1, 1), bm, 1)

' Clear mem
SelectObject NewDC, OldH
DeleteDC NewDC

PalBGRPtr = VarPtr(PalBGR(1, 1, 1, 1))
PalSize = 4 * PICW * PICH     ' Bytes

Exit Sub
'==========
DIBError:
  MsgBox "Error"
  On Error GoTo 0
End Sub

Private Sub VBRotate(zang As Single, RotFlag)

'Rotate

zCos = Cos(zang)
zSin = Sin(zang)

ixc = PICW \ 2
iyc = PICH \ 2

If RotFlag = 0 Then  ' Straight rotate

   For iy = 1 To PICH
   For ix = 1 To PICW
   
      ' For each PalBGR dest point find rotated PalBGR source point
      
      ixs = ixc + (ix - ixc) * zCos + (iy - iyc) * zSin
      iys = iyc + (iy - iyc) * zCos - (ix - ixc) * zSin
   
      If ixs > 0 And ixs <= PICW And iys > 0 And iys <= PICH Then
   
         PalBGR(1, ix, iy, 2) = PalBGR(1, ixs, iys, 1)
         PalBGR(2, ix, iy, 2) = PalBGR(2, ixs, iys, 1)
         PalBGR(3, ix, iy, 2) = PalBGR(3, ixs, iys, 1)
      
      End If
   
   Next ix
   Next iy

Else  ' Linear Anti-alias rotate

   For iy = 1 To PICH
   For ix = 1 To PICW
   
     ' For each PalBGR dest point find
     ' rotated PalBGR fractional source point
      
      xs = ixc + (ix - ixc) * zCos + (iy - iyc) * zSin
      ys = iyc + (iy - iyc) * zCos - (ix - ixc) * zSin
      
      ' Bottom left coords of bounding rectangle
      
      ixs0 = Int(xs)
      iys0 = Int(ys)
      
      ' Check that source point in range
      
      If ixs0 > 0 And ixs0 < PICW And iys0 > 0 And iys0 < PICH Then
         
         ' Color scale factors
         xsf = xs - Int(xs)
         ysf = ys - Int(ys)
         
         'ixs0->ixs0+1, iyso
         bluec = PalBGR(1, ixs0, iys0, 1)
         greenc = PalBGR(2, ixs0, iys0, 1)
         redc = PalBGR(3, ixs0, iys0, 1)
         culb = (1 - xsf) * bluec
         culg = (1 - xsf) * greenc
         culr = (1 - xsf) * redc
      
         bluec = PalBGR(1, ixs0 + 1, iys0, 1)
         greenc = PalBGR(2, ixs0 + 1, iys0, 1)
         redc = PalBGR(3, ixs0 + 1, iys0, 1)
         culb0 = culb + xsf * bluec
         culg0 = culg + xsf * greenc
         culr0 = culr + xsf * redc
         
         'ixs0->ixs0+1, iys0+1
         bluec = PalBGR(1, ixs0, iys0 + 1, 1)
         greenc = PalBGR(2, ixs0, iys0 + 1, 1)
         redc = PalBGR(3, ixs0, iys0 + 1, 1)
         culb = (1 - xsf) * bluec
         culg = (1 - xsf) * greenc
         culr = (1 - xsf) * redc
         
         bluec = PalBGR(1, ixs0 + 1, iys0 + 1, 1)
         greenc = PalBGR(2, ixs0 + 1, iys0 + 1, 1)
         redc = PalBGR(3, ixs0 + 1, iys0 + 1, 1)
         culb1 = culb + xsf * bluec
         culg1 = culg + xsf * greenc
         culr1 = culr + xsf * redc
      
         ' Weight along y axis
         culb = (1 - ysf) * culb0 + ysf * culb1
         culg = (1 - ysf) * culg0 + ysf * culg1
         culr = (1 - ysf) * culr0 + ysf * culr1
         
         ' Check for overflow
         If culb > 255 Then culb = 255
         If culg > 255 Then culg = 255
         If culr > 255 Then culr = 255
      
         ' Put colors into dest
         PalBGR(1, ix, iy, 2) = culb
         PalBGR(2, ix, iy, 2) = culg
         PalBGR(3, ix, iy, 2) = culr
   
      End If
      
   Next ix
   Next iy

End If

End Sub

'########  OPTIONS & CHECK SOUND #######################################

Private Sub optShapes_MouseUp(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)

Done = True
PIC.Picture = LoadPicture()
DoEvents

Screen.MousePointer = vbHourglass
Select Case Index
Case 0: DrawBenhamWheel
Case 1: DrawLogSpiral Index, 2, 1 ' Index, Line thickness & spiral angle(rad)
Case 2: DrawLogSpiral Index, 8, 4
Case 3: DrawColorWheel
Case 4:
   LabTitle.Caption = "Rotorelief"
   DoEvents
   PIC.Picture = LoadPicture(PathSpec$ & "Rotor.gif")
Case 5:
   LabTitle.Caption = "RedCircles Rotorelief"
   DoEvents
   PIC.Picture = LoadPicture(PathSpec$ & "RedCircles.gif")
Case 6:
   LabTitle.Caption = "Circles Rotorelief"
   DoEvents
   PIC.Picture = LoadPicture(PathSpec$ & "Circles.gif")
Case 7:
   LabTitle.Caption = "Shark Rotorelief"
   DoEvents
   PIC.Picture = LoadPicture(PathSpec$ & "Shark.gif")
End Select

Screen.MousePointer = vbDefault

GETDIBS PIC.Image

ACTIONLOOP

End Sub

Private Sub optROTAAROT_Click(Index As Integer)

RotAARot = Not RotAARot

End Sub

Private Sub optVBASM_Click(Index As Integer)

VBASM = Not VBASM

End Sub

Private Sub SBRotSpeed_Change()

txtRot = SBRotSpeed.Value

End Sub

Private Sub chkSound_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
 
 ASound = Not ASound

End Sub

'########  FORM MOVE & CHECK EXIT #######################################

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
' To move form

FormTop = Y
FormLeft = X

End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
' To move form

If Button <> 0 Then
   Me.Top = Me.Top + Y - FormTop
   Me.Left = Me.Left + X - FormLeft
End If

End Sub

Private Sub chkExit_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)

chkExit.Value = Unchecked

Done = True

sndPlaySound "", SND_PURGE

SetWindowRgn Me.hwnd, 0, False
SetWindowRgn PIC.hwnd, 0, False

Erase PalBGR
Set PIC = Nothing
Set Form1 = Nothing
Unload Me
End

End Sub

