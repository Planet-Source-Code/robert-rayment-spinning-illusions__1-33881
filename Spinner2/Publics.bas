Attribute VB_Name = "Module1"
'Module1: Publics.bas  by Robert Rayment

' Mainly to hold Publics

Option Base 1
DefLng A-W
DefSng X-Z

'--------------------------------------------------------------------------
' Shaping APIs

Public Declare Function CreateRoundRectRgn Lib "gdi32" _
(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long, ByVal X3 As Long, ByVal Y3 As Long) As Long

Public Declare Function CreateEllipticRgn Lib "gdi32" _
(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long

Public Declare Function SetWindowRgn Lib "user32" _
(ByVal hwnd As Long, ByVal hRgn As Long, ByVal bRedraw As Boolean) As Long

Public Declare Function DeleteObject Lib "gdi32" _
(ByVal hObject As Long) As Long

Public RoundReg, RoundPic

'--------------------------------------------------------------------------
' APIs for getting DIB bits to PalBGR

Public Declare Function GetDIBits Lib "gdi32" _
(ByVal aHDC As Long, ByVal hBitmap As Long, ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Any, lpBI As BITMAPINFO, ByVal wUsage As Long) As Long

Public Declare Function CreateCompatibleDC Lib "gdi32" _
(ByVal HDC As Long) As Long

Public Declare Function SelectObject Lib "gdi32" _
(ByVal HDC As Long, ByVal hObject As Long) As Long

Public Declare Function DeleteDC Lib "gdi32" _
(ByVal HDC As Long) As Long

'------------------------------------------------------------------------------

'To fill BITMAP structure
Public Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" _
(ByVal hObject As Long, ByVal Lenbmp As Long, dimbmp As Any) As Long

Public Type BITMAP
   bmType As Long              ' Type of bitmap
   bmWidth As Long             ' Pixel width
   bmHeight As Long            ' Pixel height
   bmWidthBytes As Long        ' Byte width = 3 x Pixel width
   bmPlanes As Integer         ' Color depth of bitmap
   bmBitsPixel As Integer      ' Bits per pixel, must be 16 or 24
   bmBits As Long              ' This is the pointer to the bitmap data  !!!
End Type

'NB PICTURE STORED IN MEMORY UPSIDE DOWN
'WITH INCREASING MEMORY GOING UP THE PICTURE
'bmp.bmBits points to the bottom left of the picture

Public bmp As BITMAP
'------------------------------------------------------------------------------

' Structures for StretchDIBits
Public Type BITMAPINFOHEADER ' 40 bytes
   biSize As Long
   biwidth As Long
   biheight As Long
   biPlanes As Integer
   biBitCount As Integer
   biCompression As Long
   biSizeImage As Long
   biXPelsPerMeter As Long
   biYPelsPerMeter As Long
   biClrUsed As Long
   biClrImportant As Long
End Type

Public Type BITMAPINFO
   bmiH As BITMAPINFOHEADER
   'bmiH As RGBTRIPLE            'NB Palette NOT NEEDED for 16,24 & 32-bit
End Type
Public bm As BITMAPINFO

' For transferring drawing in an integer array to Form or PicBox
Public Declare Function StretchDIBits Lib "gdi32" (ByVal HDC As Long, _
ByVal X As Long, ByVal Y As Long, _
ByVal DesW As Long, ByVal DesH As Long, _
ByVal SrcXOffset As Long, ByVal SrcYOffset As Long, _
ByVal PICWW As Long, ByVal PICHH As Long, _
lpBits As Any, lpBitsInfo As BITMAPINFO, _
ByVal wUsage As Long, ByVal dwRop As Long) As Long
'------------------------------------------------------------------------------

' For calling machine code
Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
(ByVal lpMCode As Long, _
ByVal Long1 As Long, ByVal Long2 As Long, _
ByVal Long3 As Long, ByVal Long4 As Long) As Long

Public AARotateMC() As Byte  ' Array to hold machine code
Public ptMC, ptrStruc        ' Ptrs to Machine Code & Structure

'MCode Structure
Public Type MCodeStruc
   PICW As Long
   PICH As Long
   PtrPalBGR As Long
   PalSize As Long
   zang As Single
End Type
Public MCODE As MCodeStruc

'-----------------------------------------------------------------------

' Windows API - For playing WAV files   NB Win98
Public Declare Function sndPlaySound Lib "winmm.dll" Alias "sndPlaySoundA" _
(ByVal lpszSoundName As String, ByVal uFlags As Long) As Long
        
Public Const SND_SYNC = &H0
Public Const SND_ASYNC = &H1
Public Const SND_NODEFAULT = &H2
Public Const SND_MEMORY = &H4    ' lpszSoundName points to data in memory
Public Const SND_LOOP = &H8
Public Const SND_NOSTOP = &H10
Public Const SND_PURGE = &H40

'- SND_SYNC specifies that the sound is played synchronously and the
'  function does not return until the sound ends.

'- SND_ASYNC specifies that the sound is played asynchronously and the
'  function returns immediately after beginning the sound.

'- SND_NODEFAULT specifies that if the sound cannot be found, the
'  function returns silently without playing the default sound.

'- SND_MEMORY sound played from memory (eg a String)

'- SND_LOOP specifies that the sound will continue to play continuously
'  until PlaySound is called again with the lpszSoundName$ parameter
'  set to null. You must also specify the SND_ASYNC flag to loop sounds.

'- SND_NOSTOP specifies that if a sound is currently playing, the
'  function will immediately return False without playing the requested
'  sound.

'_ SND_PURGE Stop playback

Public ASound As Boolean
' To hold data from Resource WAV file
Public WAVData As String

'-----------------------------------------------------------------------

Public PICW, PICH           ' Display picbox Width & Height (pixels)
Public PalBGR() As Byte     ' To hold 3 full palettes (12 x PICW x PICH)

Public PalSize              ' Size of 1 palette (4 x PICW x PICH)

Public Done As Boolean      ' For LOOPING
Public VBASM As Boolean      ' VB <-> ASM
Public RotAARot As Boolean    ' Rotate <-> Anti-aliasing rotate

Public Const pi# = 3.1415926535898
Public Const d2r# = pi# / 180

Public Sub Loadmcode(InFile$, MCCode() As Byte)
'Load machine code into InCode() byte array
On Error GoTo InFileErr
If Dir$(InFile$) = "" Then
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
Open InFile$ For Binary As #1
MCSize& = LOF(1)
If MCSize& = 0 Then
InFileErr:
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
ReDim MCCode(MCSize&)
Get #1, , MCCode
Close #1
On Error GoTo 0
End Sub

