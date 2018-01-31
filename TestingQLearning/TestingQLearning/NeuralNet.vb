Public Class NeuralNet
    Dim SkipTurn As Boolean = False

    Public NetworkErrors1 As New List(Of Double)
    Public NetworkErrors2 As New List(Of Double)
    Public IterationError1 As New List(Of Double)
    Public IterationError2 As New List(Of Double)
    Dim NumOfTurns As Integer = 0

    Dim NumOfWins As Integer = 0
    Dim NumOfLoss As Integer = 0
    Public OutError As New List(Of Double)

    Dim NoOfInputs As Integer = 16
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 4
    Dim OutputNeuron(NoOfOutputs - 1) As Double

    Dim NoOfHiddens As Integer = 14
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

    Dim HOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

    Dim ObjectCollision As String

    Dim NumofBatches As Integer

    Dim MaxQ As Double

    Dim ChooseBatch As Integer

    Dim Counter As Integer = 1

    Dim OutputError As Double

    Dim Rnd As New Random

    Dim p As Integer
    Dim q As Integer

    Dim QValue As Double

    Public Threshold As Double = 0.8
    Dim Action As Integer

    Public Exploration As Double
    Dim CheckAction(2, 2) As String
    Dim QList(8) As Double

    Dim LargestQ As Integer

    Dim Reward As Double

    Dim NewInputNeurons(8) As Double
    Dim CopyBatches As New List(Of Batch)
    Dim Batches As New List(Of Batch)
    Dim Batchx As Batch
    Dim ReplayBatches As New List(Of List(Of Batch))

    Dim OldInputNeurons(8) As Double

    Dim ReplayLimit As Integer = 600

    Dim NumOfDraws As Integer = 0

    Dim Gamma As Double = 1

    Dim LearningRate As Double = 0.01

    Dim InputBias(NoOfHiddens - 1) As Double

    Dim HiddenBias(NoOfOutputs - 1) As Double
    Dim Team As String


    Dim n As Integer
    Dim m As Integer

    Dim TrainingInputs(NoOfInputs - 1) As Double
    Dim Target(NoOfOutputs - 1) As Double
    Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaHiddenBias(NoOfOutputs - 1) As Double
    Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1) As Double
    Dim DeltaNetOutput(NoOfOutputs - 1) As Double
    Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaInputError(NoOfHiddens - 1) As Double
    Dim DeltaInputBias(NoOfHiddens - 1) As Double

    Dim InGame As Boolean


    Dim RewardCounter As Integer


    Dim SampleBatches As Integer
    Dim SampleBatch As Integer

    Dim FindingMaxQ As Boolean

    Dim CheckFullBoard As Boolean

    Dim TrainingState(2, 2) As String
    Dim TrainingAction As Integer

    Dim MiniBatch As New List(Of Batch)

    Dim RemoveBatch As Batch

    Dim AddBatch As Batch

    Dim Point1 As Integer
    Dim Point2 As Integer

    Dim NextMaxQ As Double

    Dim XPLayer As Integer
    Dim YPLayer As Integer

    Dim Win As Boolean = False
    Dim Lose As Boolean = False

    Sub New(T)
        Team = T
        SetIHWeights()
        SetHOWeights()
    End Sub


    Sub PlayGame()
        XPLayer = Module1.PlayerX
        YPLayer = Module1.PLayerY
        Reward = 0
        InGame = True
        '  Threshold += 0.00002
        NumOfTurns = 0

        While InGame
            Exploration = Rnd.Next(0, 50001) / 50000
            TrainingState = Module1.State.Clone
            FeedForward(Module1.State.Clone, False)
            NumOfTurns += 1
            TrainingAction = Action
            CheckWin()
            If Win Or Lose Then
                InGame = False
            End If

            FindReward()

            If InGame Then
                Exploration = 0
                FeedForward(Module1.State.Clone, True)
                NextMaxQ = MaxQ
            Else
                NextMaxQ = 0
            End If

            TrainNet()

            AddBatch = New Batch(TrainingState.Clone, TrainingAction, Module1.State.Clone, Reward)
            MiniBatch.Add(AddBatch)

            Counter = 0
            While MiniBatch.Count > ReplayLimit
                MiniBatch.RemoveAt(Counter)
                Counter += 1
            End While

            If MiniBatch.Count = ReplayLimit Then
                ChooseBatch = Rnd.Next(0, MiniBatch.Count)
                TrainNet(MiniBatch(ChooseBatch))
            End If
            If NumOfTurns > 20 Then
                Exit While
            End If

        End While


    End Sub

    Sub FindReward()
        If Win Then
            Reward += 1
        ElseIf Lose Then
            Reward += -1
        Else
            Reward += 0
        End If
    End Sub


    Sub SetInputs(Board(,) As String)

        For y = 0 To 3
            For x = 0 To 3
                If Board(x, y) = "." Then
                    InputNeurons(x + y * 3) = 0.5
                ElseIf Board(x, y) = "W" Then
                    InputNeurons(x + y * 3) = -0.5
                ElseIf Board(x, y) = "H" Then
                    InputNeurons(x + y * 3) = -1
                ElseIf Board(x, y) = "E" Then
                    InputNeurons(x + y * 3) = 1
                ElseIf Board(x, y) = "P" Then
                    InputNeurons(x + y * 3) = 0


                End If

            Next
        Next
    End Sub



    'set the ihweights
    Sub SetIHWeights()


        For x = 0 To NoOfHiddens * NoOfInputs - 1
            IHWeights(x) = Rnd.Next(-100, 101) / 100
        Next

        For x = 0 To NoOfHiddens - 1
            InputBias(x) = Rnd.Next(-100, 101) / 100

        Next

    End Sub

    'set howweights
    Sub SetHOWeights()
        For x = 0 To NoOfOutputs - 1
            For y = 0 To NoOfHiddens - 1

                HOWeights(y + x * NoOfHiddens) = Rnd.Next(-10, 11) / 100

            Next
        Next
        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) = Rnd.Next(-10, 11) / 100
        Next
    End Sub

    Sub SetHiddenNeurons(input() As Double)

        'rest hidden neurons
        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = 0
        Next

        'multiply ihweights by the input connected and add it to the hidden neuron
        For y = 0 To NoOfInputs - 1
            For x = 0 To NoOfHiddens - 1
                HiddenNeurons(x) += IHWeights(x + y * NoOfHiddens) * input(y)
            Next
        Next


        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) += InputBias(x)
        Next

        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = Tanh(HiddenNeurons(x))
        Next
    End Sub

    Sub SetOutputNeurons(hidden() As Double)
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) = 0
        Next
        For x = 0 To NoOfHiddens - 1
            For y = 0 To NoOfOutputs - 1

                OutputNeuron(y) += HOWeights(y + x * NoOfOutputs) * hidden(x)

            Next
        Next
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) += HiddenBias(x)
        Next
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) = Tanh(OutputNeuron(x))
        Next
        For x = 0 To NoOfOutputs - 1
            QList(x) = OutputNeuron(x)
        Next
    End Sub

    Function Tanh(t)
        Return Math.Tanh(t)
    End Function

    Function derivative(t)
        Return (1 - Math.Tanh(t) ^ 2)
    End Function


    Sub TrainNet(Optional TrainingBatch As Batch = Nothing)
        Dim FindMaxQ As Boolean = True
        Exploration = 0

        If TrainingBatch Is Nothing Then
            FeedForward(TrainingState.Clone, True, True)

            For x = 0 To NoOfOutputs - 1
                If x <> TrainingAction Then
                    If QList(x) = -100 Then
                        Target(x) = OutputNeuron(x)
                    Else
                        Target(x) = QList(x)
                    End If
                Else

                    Target(x) = Reward + (Gamma * NextMaxQ)
                    If Target(x) < -1 Then
                        Target(x) = -1
                    ElseIf Target(x) > 1 Then
                        Target(x) = 1
                    End If
                End If
            Next
        Else
            TrainingState = TrainingBatch.State.Clone
            Dim Training As Boolean = True

            FeedForward(TrainingBatch.NewState.Clone, True, True)
            CheckWin()
            If Win Or Lose Then
                NextMaxQ = 0
            Else
                NextMaxQ = MaxQ
            End If

            FeedForward(TrainingState.Clone, True, True)

            For x = 0 To NoOfOutputs - 1
                If x <> TrainingBatch.Action Then
                    If QList(x) = -100 Then
                        Target(x) = OutputNeuron(x)
                    Else
                        Target(x) = QList(x)
                    End If


                End If
            Next

            Target(TrainingBatch.Action) = TrainingBatch.Reward + (Gamma * NextMaxQ)
            If Target(TrainingBatch.Action) < -1 Then
                Target(TrainingBatch.Action) = -1
            ElseIf Target(TrainingBatch.Action) > 1 Then
                Target(TrainingBatch.Action) = 1
            End If
            TrainingAction = TrainingBatch.Action
        End If
        'backprop


        If Target(TrainingAction) > 1 Or Target(TrainingAction) < -1 Then
            Console.WriteLine()
        End If

        'If Team = "1" Then
        '    NetworkErrors1.Add((OutputNeuron(TrainingAction) - Target(TrainingAction)))
        '    '  NetworkErrors1.Add(Target(TrainingAction))
        '    IterationError1.Add(Module1.Iteration)
        '    OutError.Add(OutputNeuron(TrainingAction))
        'Else


        '    NetworkErrors2.Add((OutputNeuron(TrainingAction) - Target(TrainingAction)))
        '    IterationError1.Add(Module1.Iteration)
        '    OutError.Add(OutputNeuron(TrainingAction))
        '    '  NetworkErrors2.Add(Target(TrainingAction))

        'End If

        For y = 0 To NoOfHiddens
            For x = 0 To NoOfOutputs - 1
                If y = NoOfHiddens Then
                    DeltaHiddenBias(x) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * 1
                Else
                    DeltaHOWeights(x + y * NoOfOutputs) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * HiddenNeurons(y)
                End If
            Next
        Next

        For x = 0 To NoOfOutputs - 1
            DeltaNetOutput(x) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x))
        Next

        For x = 0 To NoOfHiddens - 1
            DeltaHiddenError(x) = 0
        Next

        For y = 0 To NoOfHiddens - 1
            For x = 0 To NoOfOutputs - 1
                DeltaHiddenError(y) += HOWeights(x + y * NoOfOutputs) * DeltaNetOutput(x)
            Next
        Next

        For y = 0 To NoOfInputs
            For x = 0 To NoOfHiddens - 1
                If y = NoOfInputs Then
                    DeltaInputBias(x) = DeltaHiddenError(x) * derivative(HiddenNeurons(x)) * 1
                Else
                    DeltaIHWeights(x + (y * NoOfHiddens)) = DeltaHiddenError(x) * derivative(HiddenNeurons(x)) * InputNeurons(y)
                End If
            Next
        Next

        'For x = 0 To NoOfHiddens - 1
        '    DeltaInputError(x) = 0
        'Next

        'For y = 0 To NoOfOutputs - 1
        '    For x = 0 To NoOfHiddens - 1
        '        DeltaInputError(x) += InputBias(x) * DeltaNetOutput(y)
        '    Next
        'Next

        ''For x = 0 To NoOfHiddens - 1
        ''    DeltaInputBias(x) = 0
        ''Next

        ''For x = 0 To NoOfHiddens - 1
        ''    DeltaInputBias(x) = DeltaInputError(x) * derivative(HiddenNeurons(x)) * 1
        ''Next



        'update weights
        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            HOWeights(x) -= LearningRate * DeltaHOWeights(x)
        Next

        For x = 0 To NoOfHiddens - 1
            InputBias(x) -= LearningRate * DeltaInputBias(x)
        Next

        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) -= LearningRate * DeltaHiddenBias(x)

        Next


        For x = 0 To NoOfInputs * NoOfHiddens - 1
            IHWeights(x) -= LearningRate * DeltaIHWeights(x)
        Next


    End Sub



    Sub FeedForward(Board(,) As String, DontMove As Boolean, Optional Training As Boolean = False)

        Dim PickRandom As Integer

        If Not Training Then
            Board = Module1.State.Clone
        End If


        If Exploration > Threshold Then
            Do
                PickRandom = Rnd.Next(0, 4)
                If PickRandom = 0 Then
                    XPLayer += 1
                ElseIf PickRandom = 1 Then
                    YPLayer += 1
                ElseIf PickRandom = 2 Then
                    XPLayer -= 1
                ElseIf PickRandom = 3 Then
                    YPLayer -= 1
                End If

                ObjectCollision = CheckCollision()

                Action = PickRandom


                If ObjectCollision = "W" Then
                    If PickRandom = 0 Then
                        XPLayer -= 1
                    ElseIf PickRandom = 1 Then
                        YPLayer -= 1
                    ElseIf PickRandom = 2 Then
                        XPLayer += 1
                    ElseIf PickRandom = 3 Then
                        YPLayer += 1
                    End If
                End If

            Loop Until ObjectCollision <> "W"

            If Not DontMove Then
                Module1.State(PlayerX, PLayerY) = "."
                Module1.PlayerX = XPLayer
                Module1.PLayerY = YPLayer
                Module1.State(PlayerX, PLayerY) = "P"
            End If
        Else

            SetInputs(Board)
                SetHiddenNeurons(InputNeurons)
                SetOutputNeurons(HiddenNeurons)

                LargestQ = 0

            Do
                For x = 0 To NoOfOutputs - 1
                    If QList(x) > QList(LargestQ) Then
                        LargestQ = x
                    End If
                Next


                If LargestQ = 0 Then
                        XPLayer += 1
                    ElseIf LargestQ = 1 Then
                        YPLayer += 1
                    ElseIf LargestQ = 2 Then
                        XPLayer -= 1
                    ElseIf LargestQ = 3 Then
                        YPLayer -= 1
                    End If

                    ObjectCollision = CheckCollision()

                    If ObjectCollision = "W" Then
                        If LargestQ = 0 Then
                            XPLayer -= 1
                        ElseIf LargestQ = 1 Then
                            YPLayer -= 1
                        ElseIf LargestQ = 2 Then
                            XPLayer += 1
                        ElseIf LargestQ = 3 Then
                            YPLayer += 1
                        End If
                        QList(LargestQ) = -100

                    End If



            Loop Until ObjectCollision <> "W"

            If DontMove Then
                If LargestQ = 0 Then
                    XPLayer -= 1
                ElseIf LargestQ = 1 Then
                    YPLayer -= 1
                ElseIf LargestQ = 2 Then
                    XPLayer += 1
                ElseIf LargestQ = 3 Then
                    YPLayer += 1
                End If
            End If

            If Not DontMove Then
                Module1.State(PlayerX, PLayerY) = "."
                Module1.PlayerX = XPLayer
                Module1.PLayerY = YPLayer
                Module1.State(XPLayer, YPLayer) = "P"
                Action = LargestQ
            End If



        End If


        If Not DontMove Then

        Else
            MaxQ = OutputNeuron(LargestQ)
        End If


    End Sub

    Function CheckCollision()
        If XPLayer > 3 Or XPLayer < 0 Or YPLayer > 3 Or YPLayer < 0 Then
            Return "W"
        Else
            Return Module1.State(XPLayer, YPLayer)

        End If

    End Function

    Sub CheckWin()
        Win = False
        Lose = False
        If ObjectCollision = "E" Then
            Win = True
        ElseIf ObjectCollision = "H" Then
            Lose = True
        End If

    End Sub
End Class
