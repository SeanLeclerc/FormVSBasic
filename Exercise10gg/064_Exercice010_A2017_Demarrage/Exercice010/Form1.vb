Public Class Form1



    Public champs As New ArrayList






    Public lstSexes() As String = {"F", "M", "I"}
    Public LstChiffres() As String = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}
    Public Structure Client
        Dim No As Integer
        Dim nom As String
        Dim sexe As String
        Dim province As String
        Dim codePostal As String
        Dim tel As String
        Dim naissance As String
        Dim immatricul As String
        Dim noCredi As String

    End Structure
    'Avec la collection Dictionary, les clés ne sont pas triées
    'Avec la collection SortedList, les clés sont triées automatiquement
    Public FicheClient As New Dictionary(Of Integer, Client)
    Public FicheValide As Boolean = True
    Public enregistrement As String

    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        Initialize()
    End Sub

    Private Sub Form1_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        Initialize()

    End Sub
    Public Sub Initialize()
        txtNoClient.Text = Nothing ' ""
        txtNom.Text = Nothing ' ""
        txtSexe.Text = Nothing ' ""
        txtProvince.Text = Nothing ' ""
        txtProvince.Text = Nothing ' ""
        txtPostal.Text = Nothing ' ""
        txtTelephone.Text = Nothing ' ""
        txtNaissance.Text = Nothing ' ""
        txtMatriculation.Text = Nothing ' ""
        txtCredit.Text = Nothing ' ""
    End Sub

    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click




        FicheValide = True


        If txtProvince.MaskCompleted = False Then
            txtProvince.Text = "Inscrire Province"
            txtProvince.ForeColor = Color.Red
            FicheValide = False


        End If


        If txtPostal.Text = "" Then
            txtPostal.Text = "Inscrire Code postal"
            txtPostal.ForeColor = Color.Red
            FicheValide = False

        End If

        If txtPostal.Text Like "[A-Z]#[A-Z]#[A-Z]#" = False Then
            txtPostal.Text = "Inscrire Code postal"
            txtPostal.ForeColor = Color.Red
            FicheValide = False
        End If

        If txtTelephone.MaskCompleted = False Then
            txtTelephone.Text = "Inscrire No téléphone"
            txtTelephone.ForeColor = Color.Red
            FicheValide = False

        End If

        If txtNaissance.MaskCompleted = False Then
            txtNaissance.Text = "Inscrire Date de naissance"
            txtNaissance.ForeColor = Color.Red
            FicheValide = False

        Else

            txtNaissance.TextMaskFormat = MaskFormat.ExcludePromptAndLiterals

            Dim anne = (txtNaissance.Text).Substring(0, 4)
            Dim mois = (txtNaissance.Text).Substring(4, 2)
            Dim journee = (txtNaissance.Text).Substring(6, 2)



            If CInt(anne) < 1925 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False
            End If



            If CInt(anne) > 2016 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False
            End If

            If CInt(mois) > 12 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False

            End If

            If CInt(mois) < 1 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False

            End If

            If CInt(journee) < 1 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False
            End If



            If CInt(journee) > 31 Then
                txtNaissance.Text = "Inscrire Date de naissance"
                txtNaissance.ForeColor = Color.Red
                FicheValide = False

            End If
        End If




        If txtMatriculation.Text = "" Then
            txtMatriculation.Text = "Inscrire Immatriculation"
            txtMatriculation.ForeColor = Color.Red
            FicheValide = False

        End If

        If txtMatriculation.Text Like "[A-Z][A-Z][A-Z]###" = False Then
            txtMatriculation.Text = "Inscrire Immatriculation"
            txtMatriculation.ForeColor = Color.Red
            FicheValide = False
        End If


        If txtCredit.Text = "" Then
            txtCredit.Text = "Inscrire Carte de crédit"
            txtCredit.ForeColor = Color.Red
            FicheValide = False
        End If

        If (txtCredit.Text).Length < 16 Then
            txtCredit.Text = "Inscrire Carte de crédit"
            txtCredit.ForeColor = Color.Red
            FicheValide = False
        End If


        If EstUnEntierPositif(Me.txtNoClient.Text) And Len(Me.txtNoClient.Text) = 6 Then
            Me.txtNoClient.ForeColor = Color.Black
        Else
            If Me.txtNoClient.Text = "" Then
                Me.txtNoClient.Text = "NNNNNN"
            End If
            Me.txtNoClient.ForeColor = Color.Red

            FicheValide = False
        End If
        If ValeurFournie(Me.txtNom.Text) And Not Me.txtNom.Text = "Inscrire un nom" Then
            Me.txtNom.ForeColor = Color.Black

        Else
            Me.txtNom.Text = "Inscrire un nom"
            Me.txtNom.ForeColor = Color.Red

            FicheValide = False
        End If
        If EstUnSexe((Me.txtSexe.Text).ToUpper) And Not Me.txtSexe.Text = "A" Then
            Me.txtSexe.ForeColor = Color.Black
        Else
            If Me.txtSexe.Text = "" Then
                Me.txtSexe.Text = "A"
                Me.txtSexe.ForeColor = Color.Red
                FicheValide = False
            End If
            Me.txtSexe.ForeColor = Color.Red

        End If
        If FicheValide = True Then
            Button3.Enabled = True
            Button4.Enabled = True
        End If
    End Sub
    Public Function ValeurFournie(ByRef Valeur As String) As Boolean
        ValeurFournie = True
        If IsNothing(Valeur) Or Valeur = "" Then
            ValeurFournie = False

            
        End If
    End Function
    Public Function EstUnChiffre(ByRef chiffre As String) As Boolean
        EstUnChiffre = False
        For i = 0 To LstChiffres.Length - 1
            If (chiffre) = LstChiffres(i) Then
                EstUnChiffre = True
                Exit For
            End If
        Next i
    End Function
    Public Function EstUnEntierPositif(ByRef Entier As String) As Boolean
        EstUnEntierPositif = False
        For i = 1 To Len(Entier)
            If EstUnChiffre(Mid(Entier, i, 1)) Then
                Continue For
            Else
                Exit Function
            End If
        Next i
        EstUnEntierPositif = True
    End Function
    Public Function EstUnSexe(ByRef sexe As String) As Boolean
        EstUnSexe = False
        If Len(sexe) <> 1 Then
            Exit Function
        End If
        For i = 0 To lstSexes.Length - 1
            If (sexe) = lstSexes(i) Then
                EstUnSexe = True
                Exit For
            End If
        Next i
    End Function

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click




        Dim Rapport As String
        Rapport = vbTab & vbTab & "FICHE DU CLIENT" & vbCrLf &
            vbTab & vbTab & "===============" & vbCrLf &
            "No du client : " & vbTab & Me.txtNoClient.Text & vbCrLf &
            "Nom du client: " & vbTab & Me.txtNom.Text & vbCrLf &
            "Sexe         : " & vbTab & Me.txtSexe.Text & vbCrLf &
            "Province: " & vbTab & txtProvince.Text & vbCrLf &
            "Code postal: " & vbTab & txtPostal.Text & vbCrLf &
            "No de téléphone: " & vbTab & txtTelephone.Text & vbCrLf &
            "Date naissance: " & vbTab & txtNaissance.Text & vbCrLf &
            "No d'immatriculation: " & vbTab & txtMatriculation.Text & vbCrLf &
            "No carte de crédit: " & vbTab & txtCredit.Text
        Me.TextBox11.Text = Rapport
        MsgBox(Rapport)

    End Sub

    Private Sub Button6_Click(sender As System.Object, e As System.EventArgs) Handles Button6.Click
        ' vérifier qu'une valeur a été sélectionnée dans la ComboBox avant de tenter de lire le Disctionnary
        ' vérifier que la clé existe avant dr lire et extraire les données de la fiche client du dictionnary
        If ComboBox1.Text = "" Then
            MsgBox("Erreur")
        End If

        If ComboBox1.Text = "" = False Then




            FicheClient.Remove(CInt(Me.ComboBox1.Text))
            Me.ComboBox1.Items.Remove(CInt(Me.ComboBox1.Text))
            Me.ComboBox1.Text = ""
        End If
    End Sub

    Private Sub Button5_Click(sender As System.Object, e As System.EventArgs) Handles Button5.Click
        If ComboBox1.Text = "" Then
            MsgBox("Veuillez stocker un formulaire")
        End If

        If ComboBox1.Text = "" = False Then



            Dim cle As Integer
            Dim Clients As New Client
            ' vérifier qu'une valeur a été sélectionnée dans la ComboBox avant de tenter de lire le Disctionnary
            cle = CInt(Me.ComboBox1.Text)
            ' vérifier que la clé existe avant dr lire et extraire les données de la fiche client du dictionnary
            Clients = FicheClient.Item(cle)
            Me.txtNoClient.Text = CStr(Clients.No)
            Me.txtNom.Text = Clients.nom
            Me.txtSexe.Text = Clients.sexe
            Me.txtProvince.Text = Clients.province
            Me.txtPostal.Text = Clients.codePostal
            Me.txtTelephone.Text = Clients.tel
            Me.txtNaissance.Text = Clients.naissance
            Me.txtMatriculation.Text = Clients.immatricul
            Me.txtCredit.Text = Clients.noCredi
        End If
    End Sub

    Public Sub CreerChaineDelimitee()
        enregistrement = Me.txtNoClient.Text & "|" &
        Me.txtNom.Text & "|" &
        Me.txtSexe.Text & "|" &
        Me.txtProvince.Text & "|" &
        Me.txtPostal.Text & "|" &
        Me.txtTelephone.Text & "|" &
        Me.txtNaissance.Text & "|" &
        Me.txtMatriculation.Text & "|" &
        Me.txtCredit.Text        '"|" & _
    End Sub

    Private Sub AjouterDansFichierSequentiel()
        If FicheValide Then
            CreerChaineDelimitee()
            My.Computer.FileSystem.WriteAllText("..\..\..\Exercice010.txt",
                                                                enregistrement & vbCrLf, True)
        Else
            MsgBox("Fiche invalide. Non inscrite dans le fichier.")
        End If
    End Sub

    Private Sub RelireFichierSequentielDelimite()
        Dim lineNo As Integer = 0
        Dim fileReader As System.IO.StreamReader
        Dim stringReader As String
        fileReader = My.Computer.FileSystem.OpenTextFileReader("..\..\..\Exercice010.txt")
        While Not fileReader.EndOfStream
            stringReader = fileReader.ReadLine()
            lineNo = lineNo + 1
            MsgBox(CStr(lineNo) & " : " & stringReader)
        End While
        fileReader.Close()
    End Sub

    Private Sub Button10_Click(sender As System.Object, e As System.EventArgs) Handles Button10.Click
        ' vérifier l'existence du fichier avant de tenter de le lire


        If My.Computer.FileSystem.FileExists("..\..\..\Exercice010.txt") Then
            RelireFichierSequentielDelimite()
        Else
            MsgBox("File not found.")
        End If

    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
        'Vérifier que la fiche client affichée dans le formulaire est valide

        If ComboBox1.Text = "" Then
            MsgBox("Stocker une fiche")
        Else

            CreerChaineDelimitee()
            MsgBox(enregistrement)

        End If

    End Sub




    Private Sub Button4_Click(sender As System.Object, e As System.EventArgs) Handles Button4.Click

        If FicheClient.ContainsKey(txtNoClient.Text) Then
            MsgBox("No Client existe déja")

        Else

            Dim cle As String
            Dim Clients As New Client
            cle = CStr(Me.txtNoClient.Text)
            Clients.No = CStr(Me.txtNoClient.Text)
            Clients.nom = Me.txtNom.Text
            Clients.sexe = Me.txtSexe.Text
            Clients.province = Me.txtProvince.Text
            Clients.codePostal = Me.txtPostal.Text
            Clients.tel = Me.txtTelephone.Text
            Clients.naissance = Me.txtNaissance.Text
            Clients.immatricul = Me.txtMatriculation.Text
            Clients.noCredi = Me.txtCredit.Text
            ' vérifier que la clé n'existe pas avant d'ajouter l'information dans le dictionnary
            FicheClient.Add(cle, Clients)
            Me.ComboBox1.Items.Clear()
            For Each element In FicheClient.Keys
                Me.ComboBox1.Items.Add(element)
            Next element
        End If


    End Sub

    Private Sub Button12_Click(sender As System.Object, e As System.EventArgs) Handles Button12.Click
        'http://msdn.microsoft.com/en-us/library/microsoft.visualbasic.fileio.filesystem(v=vs.100).aspx
        'Vérifier que la fiche client affichée dans le formulaire est valide
        AjouterDansFichierSequentiel()
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        Close()
    End Sub


    Private Sub RelireFichierSequentielDelimite02()

        Dim chaine As String
            Dim positionDelimiteur As Integer
            Do
                positionDelimiteur = InStr(chaine, "|")

                champs.Add(Microsoft.VisualBasic.Left(chaine, positionDelimiteur))

                chaine = Microsoft.VisualBasic.Right(chaine, Len(chaine) - positionDelimiteur)
            Loop Until InStr(chaine, "|") = 0

        champs.Add(chaine)

        Dim lineNo As Integer = 0
        Dim fileReader As System.IO.StreamReader
        Dim stringReader As String
        fileReader = My.Computer.FileSystem.OpenTextFileReader("..\..\..\Exercice010.txt")
        While Not fileReader.EndOfStream
            stringReader = fileReader.ReadLine()
            lineNo = lineNo + 1
            AfficherLesChamps(stringReader)
        End While
        fileReader.Close()
    End Sub

    Private Function AfficherLesChamps(chaine As String)

        champs.Clear()
        Dim positionDelimiteur As Integer
        Do

            positionDelimiteur = InStr(chaine, "|")

            champs.Add(Microsoft.VisualBasic.Left(chaine, positionDelimiteur))

            chaine = Microsoft.VisualBasic.Right(chaine, Len(chaine) - positionDelimiteur)
        Loop Until InStr(chaine, "|") = 0

        champs.Add(chaine)

        Dim msg As String = ""
        Dim FieldNo As Integer = 0
        For Each champ As String In champs
            FieldNo = FieldNo + 1
            msg = msg & CStr(FieldNo) & " : " & champ & vbCrLf
        Next
        MsgBox(msg)
    End Function


    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click


        If ComboBox1.Text = "" Then
            MsgBox("Rien d'enregistrer")
        Else

            RelireFichierSequentielDelimite02()





        End If





    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        If FicheClient.ContainsKey(txtNoClient.Text) Then
            MsgBox("No Client existe déja")
        Else
            RelireFichierSequentielDelimite02()
            ComboBox1.items.Clear()




        End If
    End Sub

End Class
