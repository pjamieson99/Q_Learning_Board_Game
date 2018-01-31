Public Class Batch
    Public State(,) As String
    Public Action As Integer
    Public NewState(,) As String
    Public Reward As Double

    Sub New(S(,) As String, A As Integer, NS(,) As String, R As Double)
        State = S.Clone
        Action = A
        NewState = NS.Clone
        Reward = R
    End Sub
End Class