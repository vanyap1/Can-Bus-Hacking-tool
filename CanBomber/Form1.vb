Public Class Form1
    Delegate Sub SetTextCallback(ByVal [text] As String) 'Added to prevent threading errors during receiveing of data
    Dim table As New DataTable("Table")
    Dim i As Integer
    Dim st As Integer
    Dim en As Integer
    Dim data_byte As Byte
    Dim odo_count As Byte
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim port_num As Byte

        ' Show all available COM ports.
        For Each sp As String In My.Computer.Ports.SerialPortNames
            PortNameBox.Items.Add(sp)
            port_num += 1

        Next


        PortBaudBox.Items.Add("9600")
        PortBaudBox.Items.Add("19200")
        PortBaudBox.Items.Add("38400")
        PortBaudBox.Items.Add("57600")
        PortBaudBox.Items.Add("115200")
        PortBaudBox.Items.Add("250000")
        PortNameBox.SelectedIndex = port_num - 1
        PortBaudBox.SelectedIndex = 4
        PortOpen.BackColor = Color.Lime

        table.Columns.Add("ID", Type.GetType("System.String"))
        table.Columns.Add("DLC", Type.GetType("System.String"))
        table.Columns.Add("DATA", Type.GetType("System.String"))
        table.Columns.Add("ASCII", Type.GetType("System.String"))
        table.Columns.Add("COUNT", Type.GetType("System.Int32"))

    End Sub
    Function Port_open(ByVal port As String, ByVal baud As String) As Byte
        If Not SerialPort1.IsOpen Then
            Try
                SerialPort1.PortName = port
                SerialPort1.BaudRate = CInt(Val(baud))
                SerialPort1.NewLine = vbCr
                SerialPort1.Open()
                Port_open = 0
                PortOpen.BackColor = Color.Red
                PortOpen.Text = "Close"
                PortNameBox.Enabled = False
                PortBaudBox.Enabled = False

            Catch ex As Exception
                Port_open = 1
                PortOpen.BackColor = Color.Yellow
                PortOpen.Text = "Port not available"
            End Try
        Else
            SerialPort1.Close()
            PortOpen.BackColor = Color.Lime
            PortOpen.Text = "Open"
            PortNameBox.Enabled = True
            PortBaudBox.Enabled = True
            Port_open = 2
        End If
    End Function
    Function port_write(ByVal data As String) As Byte
        Try
            If SerialPort1.IsOpen Then
                SerialPort1.WriteLine(data)
                port_write = 0
            Else
                port_write = 2
            End If
        Catch ex As Exception
            port_write = 3
        End Try
    End Function
    Private Sub SerialPort1_DataReceived(sender As Object, e As IO.Ports.SerialDataReceivedEventArgs) Handles SerialPort1.DataReceived
        Try
            ReceivedText1(SerialPort1.ReadLine())
        Catch ex As Exception
        End Try
    End Sub
    Private Sub ReceivedText1(ByVal [text] As String) 'input from ReadExisting
        Dim response As String
        Dim red As Color = Color.Red
        Dim green As Color = Color.Lime
        If Me.RdBox.InvokeRequired Then
            Dim x As New SetTextCallback(AddressOf ReceivedText1)
            Me.Invoke(x, New Object() {(text)})
        Else
            response = [text]
            Dim pack_id As String = Mid(response, 2, 3)
            Dim dlc As String = Mid(response, 5, 1)
            Dim dat As String = Mid(response, 6, 16)
            Dim count As Long

            Dim wr As Byte = 1
            If Len(response) > 2 Then
                If CheckBox1.Checked Then
                    RdBox.AppendText(pack_id & vbCrLf)
                End If


                For count = 0 To table.Rows.Count - 1
                    'Try
                    If table.Rows(count)(0) = pack_id Then
                        table.Rows(count)(4) = table.Rows(count)(4) + 1
                        table.Rows(count)(2) = dat

                        Dim str As String = ""
                        For chr_index = 1 To Len(dat) Step 2
                            str += Chr(Convert.ToByte(Mid(dat, chr_index, 2), 16))
                        Next

                        table.Rows(count)(3) = str
                        ''RdBox.AppendText(table.Rows(count)(0) & "___" & pack_id & vbCrLf)
                        wr = 0
                    End If
                    '   Catch ex As Exception
                    'wr = 1
                    ' End Try
                Next
                If wr = 1 Then
                    table.Rows.Add(pack_id, dlc, dat, "fff", 1)
                    wr = 1
                End If
                DataGridView1.DataSource = table
                DataGridView1.Columns(0).Width = 40
                DataGridView1.Columns(1).Width = 40
                DataGridView1.Columns(2).Width = 120
                DataGridView1.Columns(3).Width = 175
                DataGridView1.Columns(4).Width = 60
                DataGridView1.RowHeadersVisible = False

                ''Label2.Text = table.Rows(1)(0).ToString
            End If
            'Me.TextBox2.Text &= response & ">> HEX: (" & StringToHex(response) & "); len:(" & Len(response) & ");" & vbCrLf
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim stat As Byte = port_write(WdBox.Text)
    End Sub

    Private Sub PortOpen_Click(sender As Object, e As EventArgs) Handles PortOpen.Click
        Dim stat As Byte = Port_open(PortNameBox.SelectedItem, PortBaudBox.SelectedItem)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim stat As Byte = port_write("S6")
        stat = port_write("Z1")
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim stat As Byte = port_write("O")
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim stat As Byte = port_write("C")
    End Sub





    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        calc_cBoxes(id_calc(0))

    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click



        calc_cBoxes(id_calc(1))
    End Sub

    Function speed_calc(ByVal dat As Long, ByVal odoo As Byte) As String
        Dim id As Byte = &HB4
        Dim sym As Long = id + 8 + odoo + dat + HScrollBar3.Value
        Dim crc As String = sym.ToString("x2")
        'odoo.ToString("X2")
        speed_calc = "t0B4800000000" + odoo.ToString("X2") + dat.ToString("X2") + HScrollBar3.Value.ToString("X2") + crc.Substring(crc.Length - 2, 2)
        Dim stat As Byte = port_write(TextBox5.Text)
        stat = port_write("t3BB8FF00000000000000")
        'stat = port_write("t3B78FF00000000000000")
        stat = port_write("t24785500000000000000")
        'stat = port_write("t41280000000000000000")
        stat = port_write("t3B988000000000000000")
        'stat = port_write("t39480000000000000000")
        'stat = port_write("t3F68FFFFFFFFFFFFFFFF")
        'stat = port_write("t3968AAAAAAAAAAAAAAAA")

    End Function

    Function id_calc(ByVal fun As Byte) As String
        st = Convert.ToInt32(TextBox1.Text, 16)
        en = Convert.ToInt32(TextBox2.Text, 16)

        If fun < 4 Then
            If fun = 0 Then
                i += 1
            Else
                i -= 1
            End If
        End If
        If i = en Then

            If data_byte = 255 Then
                data_byte = 0
                Timer1.Enabled = False
                sel4.Checked = True

            Else
                data_byte += 1
            End If
            Timer1.Enabled = False
            sel4.Checked = True
            st = Convert.ToInt32(TextBox1.Text, 16)
            en = Convert.ToInt32(TextBox2.Text, 16)
            i = st
        End If

        Dim str As String = i.ToString("X3")
        'id_calc = str
        'TextBox5.Text = "t" & str & "8" & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2") & data_byte.ToString("X2")
        TextBox5.Text = "t" & str & "8" & TextBox6.Text
        id_calc = "t" & str & (Len(TextBox6.Text) / 2) & TextBox6.Text
        Dim stat As Byte = port_write(id_calc)
    End Function
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        st = Convert.ToInt32(TextBox1.Text, 16)
        en = Convert.ToInt32(TextBox2.Text, 16)
        i = st
        data_byte = 0

    End Sub

    Function calc_cBoxes(ByVal instr As String) As String
        'RdBox.AppendText(instr & vbCrLf)
        't0018FFFFFFFFFFFFFFFF
        Dim byte_(8) As Byte
        Dim byte_num As Byte = 1
        Dim t As Integer
        Dim s As String
        t = 0
        For i As Byte = 6 To 21 Step 2
            byte_(byte_num) = Convert.ToByte(Mid(instr, i, 2), 16)
            byte_num += 1
        Next
        'Byte 1 group
        s = Convert.ToString(byte_(1), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray1.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next

        'Byte 2 group
        s = Convert.ToString(byte_(2), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray2.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 3 group
        s = Convert.ToString(byte_(3), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray3.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 4 group
        s = Convert.ToString(byte_(4), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray4.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 5 group
        s = Convert.ToString(byte_(5), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray5.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 6 group
        s = Convert.ToString(byte_(6), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray6.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 7 group
        s = Convert.ToString(byte_(7), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray7.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next
        'Byte 8 group
        s = Convert.ToString(byte_(8), 2)
        If Len(s) < 8 Then
            Do
                s = "0" + s
            Loop Until Len(s) = 8
        End If
        byte_num = 8
        For Each chk As CheckBox In BitArray8.Controls
            chk.Checked = Val(Mid(s, byte_num, 1))
            byte_num -= 1
        Next

    End Function
    Function calc_msg() As String
        Dim byte_(8) As Byte
        Dim t As Integer
        Dim out_str As String
        t = 0
        For Each chk As CheckBox In BitArray1.Controls
            If chk.Checked = True Then
                byte_(1) = byte_(1) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray2.Controls
            If chk.Checked = True Then
                byte_(2) = byte_(2) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray3.Controls
            If chk.Checked = True Then
                byte_(3) = byte_(3) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray4.Controls
            If chk.Checked = True Then
                byte_(4) = byte_(4) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray5.Controls
            If chk.Checked = True Then
                byte_(5) = byte_(5) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray6.Controls
            If chk.Checked = True Then
                byte_(6) = byte_(6) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray7.Controls
            If chk.Checked = True Then
                byte_(7) = byte_(7) + 2 ^ t
            End If
            t = t + 1
        Next
        t = 0
        For Each chk As CheckBox In BitArray8.Controls
            If chk.Checked = True Then
                byte_(8) = byte_(8) + 2 ^ t
            End If
            t = t + 1
        Next
        out_str = byte_(1).ToString("X2")
        out_str += byte_(2).ToString("X2")
        out_str += byte_(3).ToString("X2")
        out_str += byte_(4).ToString("X2")
        out_str += byte_(5).ToString("X2")
        out_str += byte_(6).ToString("X2")
        out_str += byte_(7).ToString("X2")
        out_str += byte_(8).ToString("X2")
        TextBox6.Text = out_str
        Dim tmp_str As String = id_calc(5)
    End Function

    Private Sub b17_CheckedChanged(sender As Object, e As EventArgs) Handles b17.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b16_CheckedChanged(sender As Object, e As EventArgs) Handles b16.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b15_CheckedChanged(sender As Object, e As EventArgs) Handles b15.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b14_CheckedChanged(sender As Object, e As EventArgs) Handles b14.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b13_CheckedChanged(sender As Object, e As EventArgs) Handles b13.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b12_CheckedChanged(sender As Object, e As EventArgs) Handles b12.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b11_CheckedChanged(sender As Object, e As EventArgs) Handles b11.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b10_CheckedChanged(sender As Object, e As EventArgs) Handles b10.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b27_CheckedChanged(sender As Object, e As EventArgs) Handles b27.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b26_CheckedChanged(sender As Object, e As EventArgs) Handles b26.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b25_CheckedChanged(sender As Object, e As EventArgs) Handles b25.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b24_CheckedChanged(sender As Object, e As EventArgs) Handles b24.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b23_CheckedChanged(sender As Object, e As EventArgs) Handles b23.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b22_CheckedChanged(sender As Object, e As EventArgs) Handles b22.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b21_CheckedChanged(sender As Object, e As EventArgs) Handles b21.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b20_CheckedChanged(sender As Object, e As EventArgs) Handles b20.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b37_CheckedChanged(sender As Object, e As EventArgs) Handles b37.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b36_CheckedChanged(sender As Object, e As EventArgs) Handles b36.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b35_CheckedChanged(sender As Object, e As EventArgs) Handles b35.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b34_CheckedChanged(sender As Object, e As EventArgs) Handles b34.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b33_CheckedChanged(sender As Object, e As EventArgs) Handles b33.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b32_CheckedChanged(sender As Object, e As EventArgs) Handles b32.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b31_CheckedChanged(sender As Object, e As EventArgs) Handles b31.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b30_CheckedChanged(sender As Object, e As EventArgs) Handles b30.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b47_CheckedChanged(sender As Object, e As EventArgs) Handles b47.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b46_CheckedChanged(sender As Object, e As EventArgs) Handles b46.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b45_CheckedChanged(sender As Object, e As EventArgs) Handles b45.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b44_CheckedChanged(sender As Object, e As EventArgs) Handles b44.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b43_CheckedChanged(sender As Object, e As EventArgs) Handles b43.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b42_CheckedChanged(sender As Object, e As EventArgs) Handles b42.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b41_CheckedChanged(sender As Object, e As EventArgs) Handles b41.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b40_CheckedChanged(sender As Object, e As EventArgs) Handles b40.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b57_CheckedChanged(sender As Object, e As EventArgs) Handles b57.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b56_CheckedChanged(sender As Object, e As EventArgs) Handles b56.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b55_CheckedChanged(sender As Object, e As EventArgs) Handles b55.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b54_CheckedChanged(sender As Object, e As EventArgs) Handles b54.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b53_CheckedChanged(sender As Object, e As EventArgs) Handles b53.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b52_CheckedChanged(sender As Object, e As EventArgs) Handles b52.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b51_CheckedChanged(sender As Object, e As EventArgs) Handles b51.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b50_CheckedChanged(sender As Object, e As EventArgs) Handles b50.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b67_CheckedChanged(sender As Object, e As EventArgs) Handles b67.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b66_CheckedChanged(sender As Object, e As EventArgs) Handles b66.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b65_CheckedChanged(sender As Object, e As EventArgs) Handles b65.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b64_CheckedChanged(sender As Object, e As EventArgs) Handles b64.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b63_CheckedChanged(sender As Object, e As EventArgs) Handles b63.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b62_CheckedChanged(sender As Object, e As EventArgs) Handles b62.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b61_CheckedChanged(sender As Object, e As EventArgs) Handles b61.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b60_CheckedChanged(sender As Object, e As EventArgs) Handles b60.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b77_CheckedChanged(sender As Object, e As EventArgs) Handles b77.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b76_CheckedChanged(sender As Object, e As EventArgs) Handles b76.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b75_CheckedChanged(sender As Object, e As EventArgs) Handles b75.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b74_CheckedChanged(sender As Object, e As EventArgs) Handles b74.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b73_CheckedChanged(sender As Object, e As EventArgs) Handles b73.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b72_CheckedChanged(sender As Object, e As EventArgs) Handles b72.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b71_CheckedChanged(sender As Object, e As EventArgs) Handles b71.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b70_CheckedChanged(sender As Object, e As EventArgs) Handles b70.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b87_CheckedChanged(sender As Object, e As EventArgs) Handles b87.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b86_CheckedChanged(sender As Object, e As EventArgs) Handles b86.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b85_CheckedChanged(sender As Object, e As EventArgs) Handles b85.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b84_CheckedChanged(sender As Object, e As EventArgs) Handles b84.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b83_CheckedChanged(sender As Object, e As EventArgs) Handles b83.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b82_CheckedChanged(sender As Object, e As EventArgs) Handles b82.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b81_CheckedChanged(sender As Object, e As EventArgs) Handles b81.CheckedChanged
        Dim result As String = calc_msg()
    End Sub
    Private Sub b80_CheckedChanged(sender As Object, e As EventArgs) Handles b80.CheckedChanged
        Dim result As String = calc_msg()
    End Sub








    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        Dim stat As Byte = port_write(TextBox5.Text)
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        TextBox3.Text = id_calc(0)

    End Sub

    Private Sub TextBox6_TextChanged(sender As Object, e As EventArgs) Handles TextBox6.TextChanged
        Label11.Text = TextBox6.Text.Length.ToString
    End Sub

    Private Sub sel1_CheckedChanged(sender As Object, e As EventArgs) Handles sel1.CheckedChanged
        Timer1.Enabled = True
        Timer2.Enabled = False
        Timer3.Enabled = False
    End Sub

    Private Sub sel4_CheckedChanged(sender As Object, e As EventArgs) Handles sel4.CheckedChanged
        Timer1.Enabled = False
        Timer2.Enabled = False
        Timer3.Enabled = False
    End Sub

    Private Sub sel2_CheckedChanged(sender As Object, e As EventArgs) Handles sel2.CheckedChanged
        Timer1.Enabled = False
        Timer2.Enabled = False
        Timer3.Enabled = True
    End Sub


    Private Sub sel3_CheckedChanged(sender As Object, e As EventArgs) Handles sel3.CheckedChanged
        Timer1.Enabled = False

        Timer3.Enabled = False
        Timer2.Interval = HScrollBar2.Value
        Timer2.Enabled = True
    End Sub

    Private Sub HScrollBar1_Scroll(sender As Object, e As ScrollEventArgs) Handles HScrollBar1.Scroll
        Label12.Text = HScrollBar1.Value.ToString()

    End Sub

    Private Sub HScrollBar2_Scroll(sender As Object, e As ScrollEventArgs) Handles HScrollBar2.Scroll
        Label13.Text = HScrollBar2.Value.ToString()
        Timer2.Interval = HScrollBar2.Value
    End Sub

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick
        TextBox5.Text = speed_calc(HScrollBar1.Value, odo_count)
        If ODOO.Checked Then


            Try
                odo_count += 20
            Catch ex As Exception
                odo_count = 0
            End Try
        End If
    End Sub

    Private Sub HScrollBar3_Scroll(sender As Object, e As ScrollEventArgs) Handles HScrollBar3.Scroll

    End Sub

    Private Sub GroupBox1_Enter(sender As Object, e As EventArgs) Handles GroupBox1.Enter

    End Sub

    Private Sub MenuStrip1_ItemClicked(sender As Object, e As ToolStripItemClickedEventArgs) Handles MenuStrip1.ItemClicked

    End Sub
End Class
