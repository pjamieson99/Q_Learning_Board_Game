Module Module1
    Public State(3, 3) As String
    Dim Rnd As New Random
    Public HoleX As Integer

    Public HoleY As Integer
    Public ExitX As Integer
    Public ExitY As Integer
    Public WallX As Integer
    Public WallY As Integer
    Public PlayerX As Integer
    Public PLayerY As Integer
    Dim Net As NeuralNet
    Dim Iterations As Integer = 0
    Sub MakeMap()
        For x = 0 To 3
            For y = 0 To 3
                State(x, y) = "."
            Next
        Next
        HoleY = Rnd.Next(0, 4)
        HoleX = Rnd.Next(0, 4)

        WallX = Rnd.Next(0, 4)
        WallY = Rnd.Next(0, 4)

        While HoleX = WallX And HoleY = WallY
            WallX = Rnd.Next(0, 4)
            WallY = Rnd.Next(0, 4)
        End While

        ExitY = Rnd.Next(0, 4)
        ExitX = Rnd.Next(0, 4)

        While HoleX = ExitX And HoleY = ExitY Or WallX = ExitX And WallY = ExitY
            ExitX = Rnd.Next(0, 4)
            ExitY = Rnd.Next(0, 4)
        End While

        PLayerY = Rnd.Next(0, 4)
        PlayerX = Rnd.Next(0, 4)

        While HoleX = PlayerX And HoleY = PLayerY Or WallX = PlayerX And WallY = PLayerY Or ExitX = PlayerX And ExitY = PLayerY
            PlayerX = Rnd.Next(0, 4)
            PLayerY = Rnd.Next(0, 4)
        End While

        State(WallX, WallY) = "W"
        State(HoleX, HoleY) = "H"
        State(ExitX, ExitY) = "E"
        State(PlayerX, PLayerY) = "P"
    End Sub

    Sub Main()
        Net = New NeuralNet("1")
        Do
            MakeMap()
            Net.PlayGame()
            Iterations += 1
        Loop Until Iterations = 50000
        Test()
    End Sub

    Sub Test()
        MakeMap()
        Net.Threshold = 2
        Net.PlayGame()
    End Sub
End Module
