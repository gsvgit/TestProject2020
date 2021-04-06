using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Platform;
using Avalonia.Dialogs;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
using Avalonia.Media.Imaging;
using Avalonia.Native;
using Avalonia.Platform;
using Avalonia.Threading;
using AvaloniaEdit.CodeCompletion;
using AvaloniaEdit.Document;
using AvaloniaEdit.Editing;
using AvaloniaEdit.Highlighting;
using AvaloniaEdit.Rendering;

namespace AvaloniaEdit.Demo
{
    using Pair = KeyValuePair<int, IControl>;

    public class MainWindow : Window
    {
        private readonly AvaloniaEdit.TextEditor _textEditor;
        private CompletionWindow _completionWindow;
        private OverloadInsightWindow _insightWindow;
        private Button _openFileBtn;
        private Button _saveFileBtn;
        private readonly Button _runBtn;
        private readonly TextBlock _outText;
        private readonly TextBlock _executionStatus;
        private readonly TextBox _console;
        private ElementGenerator _generator = new ElementGenerator();

        private string _currentFile = "";

        private bool isSuccessfulRun = true;

        public MainWindow()
        {
            InitializeComponent();

            _textEditor = this.FindControl<TextEditor>("Editor");
            _textEditor.Background = Brushes.Transparent;
            _textEditor.ShowLineNumbers = true;
            _textEditor.SyntaxHighlighting = HighlightingManager.Instance.GetDefinition("C#");
            _textEditor.TextArea.TextEntered += textEditor_TextArea_TextEntered;
            _textEditor.TextArea.TextEntering += textEditor_TextArea_TextEntering;
            _textEditor.TextArea.IndentationStrategy = new Indentation.CSharp.CSharpIndentationStrategy();

            _outText = this.FindControl<TextBlock>("outText");
            _executionStatus = this.FindControl<TextBlock>("executionStatus");
            _console = this.FindControl<TextBox>("console");
            _console.IsReadOnly = true;
            Interpreter.Printed.Subscribe(PrintToConsole);
            Interpreter.NewRuntimeException.Subscribe(PrintErrorToConsole);


            _openFileBtn = this.FindControl<Button>("openFileBtn");
            _openFileBtn.Click += _openFileBtn_Click;

            _saveFileBtn = this.FindControl<Button>("saveFileBtn");
            _saveFileBtn.Click += SaveFileBtnClick; ;

            _runBtn = this.FindControl<Button>("runBtn");
            _runBtn.Click += RunBtnClick; ;

            _textEditor.TextArea.TextView.ElementGenerators.Add(_generator);

            this.AddHandler(PointerWheelChangedEvent, (o, i) =>
            {
                if (i.KeyModifiers != KeyModifiers.Control) return;
                if (i.Delta.Y > 0) _textEditor.FontSize++;
                else _textEditor.FontSize = _textEditor.FontSize > 1 ? _textEditor.FontSize - 1 : 1;
            }, RoutingStrategies.Bubble, true);
        }

        private void RunBtnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            //var code = _textEditor.Text;
            //var ast = RegexpParser.parseRegexpFromString(code);
            //Interpreter.run(ast);
            try
            {
                _runBtn.IsEnabled = false;
                isSuccessfulRun = true;
                _executionStatus.Background = Brushes.Yellow;
                _console.Text = "Execution started:\n";
                var code = this._textEditor.Text;
                var ast = RegexpParser.parseRegexpFromString(code);
                var task = new Task(() => Interpreter.run(ast));
                task.ContinueWith(t =>
                    Dispatcher.UIThread.Post(() =>
                    {
                        _runBtn.IsEnabled = true;
                        if (isSuccessfulRun) _executionStatus.Background = Brushes.Green;
                        _console.Text += "\n" + "Execution finished.";
                    }));
                task.Start();
            }
            catch (Exception exception)
            {
                _console.Text += exception.Message;
                _console.Text += "\n" + "Execution finished.";
                _executionStatus.Background = Brushes.Red;
                _runBtn.IsEnabled = true;
                isSuccessfulRun = false;
            }
        }

        private void PrintToConsole(string msg)
        {
            Dispatcher.UIThread.Post(() =>
                _console.Text = _console.Text + "\n" + msg);
        }

        private void PrintErrorToConsole(string msg)
        {
            Dispatcher.UIThread.Post(() =>
            {
                isSuccessfulRun = false;
                _console.Text = _console.Text + "\n" + msg;
                _executionStatus.Background = Brushes.Red;});
        }
        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        async void _openFileBtn_Click(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            var openDialog = new OpenFileDialog();
            openDialog.AllowMultiple = false;
            var path = await openDialog.ShowAsync(this);
            if (path != null)
            {
                var text = System.IO.File.ReadAllText(path[0]);
                _currentFile = path[0];
                _textEditor.Text = text;
            };
        }

        void SaveFileBtnClick(object sender, Avalonia.Interactivity.RoutedEventArgs e)
        {
            //TODO: delete elements using back key
            //_generator.controls.Clear();
            //_textEditor.TextArea.TextView.Redraw();
            _outText.Text = "Saved!";
        }

        void textEditor_TextArea_TextEntering(object sender, TextInputEventArgs e)
        {
            if (e.Text.Length > 0 && _completionWindow != null)
            {
                if (!char.IsLetterOrDigit(e.Text[0]))
                {
                    // Whenever a non-letter is typed while the completion window is open,
                    // insert the currently selected element.
                    _completionWindow.CompletionList.RequestInsertion(e);
                }
            }

            _insightWindow?.Hide();

            // Do not set e.Handled=true.
            // We still want to insert the character that was typed.
        }

        void textEditor_TextArea_TextEntered(object sender, TextInputEventArgs e)
        {
            if (e.Text == ".")
            {

                _completionWindow = new CompletionWindow(_textEditor.TextArea);
                _completionWindow.Closed += (o, args) => _completionWindow = null;

                var data = _completionWindow.CompletionList.CompletionData;
                data.Add(new MyCompletionData("Item1"));
                data.Add(new MyCompletionData("Item2"));
                data.Add(new MyCompletionData("Item3"));
                data.Add(new MyCompletionData("Item4"));
                data.Add(new MyCompletionData("Item5"));
                data.Add(new MyCompletionData("Item6"));
                data.Add(new MyCompletionData("Item7"));
                data.Add(new MyCompletionData("Item8"));
                data.Add(new MyCompletionData("Item9"));
                data.Add(new MyCompletionData("Item10"));
                data.Add(new MyCompletionData("Item11"));
                data.Add(new MyCompletionData("Item12"));
                data.Add(new MyCompletionData("Item13"));


                _completionWindow.Show();
            }
            else if (e.Text == "(")
            {
                _insightWindow = new OverloadInsightWindow(_textEditor.TextArea);
                _insightWindow.Closed += (o, args) => _insightWindow = null;

                _insightWindow.Provider = new MyOverloadProvider(new[]
                {
                    ("Method1(int, string)", "Method1 description"),
                    ("Method2(int)", "Method2 description"),
                    ("Method3(string)", "Method3 description"),
                });

                _insightWindow.Show();
            }
        }

        private class MyOverloadProvider : IOverloadProvider
        {
            private readonly IList<(string header, string content)> _items;
            private int _selectedIndex;

            public MyOverloadProvider(IList<(string header, string content)> items)
            {
                _items = items;
                SelectedIndex = 0;
            }

            public int SelectedIndex
            {
                get => _selectedIndex;
                set
                {
                    _selectedIndex = value;
                    OnPropertyChanged();
                    // ReSharper disable ExplicitCallerInfoArgument
                    OnPropertyChanged(nameof(CurrentHeader));
                    OnPropertyChanged(nameof(CurrentContent));
                    // ReSharper restore ExplicitCallerInfoArgument
                }
            }

            public int Count => _items.Count;
            public string CurrentIndexText => null;
            public object CurrentHeader => _items[SelectedIndex].header;
            public object CurrentContent => _items[SelectedIndex].content;

            public event PropertyChangedEventHandler PropertyChanged;

            private void OnPropertyChanged([CallerMemberName] string propertyName = null)
            {
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        public class MyCompletionData : ICompletionData
        {
            public MyCompletionData(string text)
            {
                Text = text;
            }

            public IBitmap Image => null;

            public string Text { get; }

            // Use this property if you want to show a fancy UIElement in the list.
            public object Content => Text;

            public object Description => "Description for " + Text;

            public double Priority { get; } = 0;

            public void Complete(TextArea textArea, ISegment completionSegment,
                EventArgs insertionRequestEventArgs)
            {
                textArea.Document.Replace(completionSegment, Text);
            }
        }

        class ElementGenerator : VisualLineElementGenerator, IComparer<Pair>
        {
            public List<Pair> controls = new List<Pair>();

            /// <summary>
            /// Gets the first interested offset using binary search
            /// </summary>
            /// <returns>The first interested offset.</returns>
            /// <param name="startOffset">Start offset.</param>
            public override int GetFirstInterestedOffset(int startOffset)
            {
                int pos = controls.BinarySearch(new Pair(startOffset, null), this);
                if (pos < 0)
                    pos = ~pos;
                if (pos < controls.Count)
                    return controls[pos].Key;
                else
                    return -1;
            }

            public override VisualLineElement ConstructElement(int offset)
            {
                int pos = controls.BinarySearch(new Pair(offset, null), this);
                if (pos >= 0)
                    return new InlineObjectElement(0, controls[pos].Value);
                else
                    return null;
            }

            int IComparer<Pair>.Compare(Pair x, Pair y)
            {
                return x.Key.CompareTo(y.Key);
            }
        }
    }
}
