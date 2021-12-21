import Editor, { Monaco } from "@monaco-editor/react";
import React from "react";
import { TerminalData } from "./TerminalData";
import { TerminalInput } from "./TerminalInput";
import { editor, KeyMod, KeyCode } from "monaco-editor";
import { kLanguageId, registerLanguage } from "../monaco/scheme";
import { defineThemes } from "../monaco/solarized";
import { EditorThemeContext, ThemeContext } from "./ThemeProvider";
import { Copy, Cut, Open, Paste, Save } from "../icons/copy";
import { IconButton } from "./IconButton";

interface TerminalProps {
  welcomeMessage?: React.ReactNode;
  prompt: string;
  pause: boolean;
  autofocus: boolean;
  fontSize: number;
  onInput: (str: string) => Promise<string>;
}

interface TerminalState {
  output: string[];
  history: string[];
  hidx: number;
  input: string;
  cached: string;
  editing: boolean;
  line: string;
  canPaste: boolean;
}

const kDefaultState: TerminalState = {
  output: [],
  history: [],
  hidx: 0,
  input: "",
  cached: "",
  editing: false,
  line: "",
  canPaste: false,
};

export const Terminal: React.FunctionComponent<TerminalProps> = (props) => {
  const [state, setState] = React.useState<TerminalState>({ ...kDefaultState });
  const theme = React.useContext(ThemeContext);
  const editorTheme = React.useContext(EditorThemeContext) || theme;

  const onEnter = async (prompt: string, text: string) => {
    const cmd = text;
    const history = [...state.history];

    // update history
    const hidx = history.findIndex((el) => el === cmd);
    if (hidx >= 0) {
      history.splice(hidx, 1);
    }
    history.push(cmd);

    const output = [...state.output, prompt + cmd];
    const result = await props.onInput(cmd);
    if (result.length > 0) {
      output.push(result);
    }

    setState({
      ...state,
      input: "",
      history: history,
      hidx: history.length,
      output: output,
      cached: "",
    });
  };

  const onUp = () => {
    if (state.hidx <= 0) {
      return;
    }
    setState({
      ...state,
      history: state.history,
      hidx: state.hidx - 1,
      cached: state.hidx == state.history.length ? state.input : state.cached,
      input: state.history[state.hidx - 1],
    });
  };
  const onDown = () => {
    if (state.hidx >= state.history.length) {
      return;
    } else if (state.hidx == state.history.length - 1) {
      setState({
        ...state,
        hidx: state.hidx + 1,
        input: state.cached,
        cached: "",
      });
    } else {
      setState({
        ...state,
        hidx: state.hidx + 1,
        input: state.history[state.hidx + 1],
      });
    }
  };

  const onEscape = (text: string) => {
    setState({
      ...state,
      editing: true,
      line: text,
    });
  };

  const editorRef = React.useRef<editor.IStandaloneCodeEditor>();

  const beforeEditorMount = (monaco: Monaco) => {
    registerLanguage(monaco);
    defineThemes(monaco);
  };

  const clipboardChange = async () => {
    try {
      const content = await navigator.clipboard.readText();
      setState({ ...state, canPaste: !!content && content.length > 0 });
    } catch (err) {
      setState({ ...state, canPaste: false });
    }
  };

  const onEditorMount = (editor: editor.IStandaloneCodeEditor) => {
    editorRef.current = editor;
    // editor.updateOptions({ minimap: { enabled: false } });
    editor.addAction({
      id: "end-editing-scheme",
      label: "End Editing",
      keybindings: [
        KeyMod.CtrlCmd | KeyCode.KeyE,
        KeyMod.chord(
          KeyMod.CtrlCmd | KeyCode.KeyK,
          KeyMod.CtrlCmd | KeyCode.KeyD
        ),
      ],
      contextMenuGroupId: "navigation",
      contextMenuOrder: 1.5,
      run: (ed) => {
        setState({
          ...state,
          editing: false,
          line: "",
          input: ed.getValue(),
        });
      },
    });
    editor.updateOptions({ fontSize: props.fontSize });
    editor.focus();
  };

  React.useEffect(() => {
    if (editorRef.current) {
      const editor = editorRef.current;
      editor.updateOptions({ fontSize: props.fontSize });
    }
  }, [props.fontSize]);

  React.useEffect(() => {
    if (state.editing) {
      navigator.clipboard.addEventListener("clipboardchange", clipboardChange);
      navigator.clipboard
        .readText()
        .then((content) =>
          setState({ ...state, canPaste: !!content && content.length > 0 })
        );
    } else {
      navigator.clipboard.removeEventListener(
        "clipboardchange",
        clipboardChange
      );
    }
  }, [state.editing]);

  const onDoneEditing = () => {
    setState({
      ...state,
      editing: false,
      line: "",
      input: editorRef.current ? editorRef.current.getValue() : state.line,
    });
  };

  if (state.editing && !props.pause) {
    return (
      <div
        style={{
          color: editorTheme.foreground,
          background: editorTheme.background,
        }}
      >
        <div
          style={{
            display: "grid",
            background: editorTheme.boldBackground,
            padding: "0.25em",
            gridTemplateColumns: "auto auto 1fr auto",
          }}
        >
          <span
            style={{
              margin: "auto 1em",
              fontWeight: 500,
            }}
          >
            Editing current line
          </span>
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "3fr 3fr 3fr 1fr 3fr 3fr",
              columnGap: "0.25em",
              alignSelf: "center",
            }}
          >
            <IconButton
              size={props.fontSize * 2}
              title="Cut"
              onClick={() => {
                const editor = editorRef.current;
                if (!editor) {
                  return;
                }
                editor.focus();
                const selection = editor.getSelection();
                if (!selection || selection.isEmpty()) {
                  navigator.clipboard.writeText("");
                  return;
                }
                const data = editor.getModel()?.getValueInRange(selection);
                navigator.clipboard.writeText(data || "");
                editor.executeEdits("clipboard", [
                  {
                    range: selection,
                    text: "",
                    forceMoveMarkers: true,
                  },
                ]);
              }}
            >
              <Cut />
            </IconButton>
            <IconButton
              size={props.fontSize * 2}
              title="Copy"
              onClick={() => {
                const editor = editorRef.current;
                if (!editor) {
                  return;
                }
                editor.focus();
                const selection = editor.getSelection();
                if (!selection || selection.isEmpty()) {
                  navigator.clipboard.writeText("");
                  return;
                }
                const data = editor.getModel()?.getValueInRange(selection);
                navigator.clipboard.writeText(data || "");
              }}
            >
              <Copy />
            </IconButton>
            <IconButton
              size={props.fontSize * 2}
              title="Paste"
              disabled={!state.canPaste}
              onClick={() => {
                const editor = editorRef.current;
                if (!editor) {
                  return;
                }
                editor.focus();
                navigator.clipboard.readText().then((v) => {
                  const selection = editor.getSelection();
                  if (!selection) {
                    return;
                  }
                  editor.executeEdits("clipboard", [
                    {
                      range: selection,
                      text: v,
                      forceMoveMarkers: true,
                    },
                  ]);
                });
              }}
            >
              <Paste />
            </IconButton>
            <div />
            <IconButton
              size={props.fontSize * 2}
              title="Open"
              onClick={() => {
                const editor = editorRef.current;
                if (!editor) {
                  return;
                }
                const input = document.createElement("input");
                input.type = "file";
                input.multiple = false;
                input.accept =
                  ".scm,text/plain,text/x-script.scheme,text/x-scheme";
                input.addEventListener("change", async () => {
                  const files = input.files;
                  if (!files || files.length == 0) {
                    return;
                  }
                  const file = files.item(0);
                  if (!file) {
                    return;
                  }
                  const text = await file.text();
                  editor.focus();
                  const selection = editor.getSelection();
                  if (!selection) {
                    editor.setValue(text);
                    return;
                  } else {
                    editor.executeEdits("file-open", [
                      { range: selection, text: text, forceMoveMarkers: true },
                    ]);
                  }
                });
                input.click();
              }}
            >
              <Open />
            </IconButton>
            <IconButton
              size={props.fontSize * 2}
              title="Save"
              onClick={() => {
                const editor = editorRef.current;
                if (!editor) {
                  return;
                }

                const content = editor.getValue();
                const blob = new Blob([content], { type: "text/plain" });
                const anchor = document.createElement("a");
                anchor.href = window.URL.createObjectURL(blob);
                anchor.download = "untitled.scm";
                anchor.click();
              }}
            >
              <Save />
            </IconButton>
          </div>
          <div />
          <button
            style={{
              minWidth: "6em",
              minHeight: "2em",
              margin: "0.25em",
              background: editorTheme.blue,
              border: `1px solid ${theme.base00}`,
              borderRadius: "0.25em",
              color: editorTheme.foreground,
            }}
            title={"Finish editing\nShortcuts:\n  · Ctrl+K Ctrl+D\n  · Ctrl+E"}
            onClick={() => onDoneEditing()}
          >
            Done
          </button>
        </div>
        <Editor
          height="90vh"
          theme={editorTheme.name}
          defaultLanguage={kLanguageId}
          defaultValue={state.line}
          onMount={(editor: editor.IStandaloneCodeEditor) =>
            onEditorMount(editor)
          }
          beforeMount={(instance: Monaco) => beforeEditorMount(instance)}
        />
      </div>
    );
  } else {
    return (
      <div
        style={{
          backgroundColor: theme.background,
          color: theme.foreground,
          fontFamily: "'Source Code Pro', monospace",
          padding: "0.5em",
          minWidth: "40rem",
        }}
      >
        {props.welcomeMessage}
        <TerminalData text={state.output} />
        <TerminalInput
          prompt={props.prompt}
          readonly={props.pause}
          value={state.input}
          autofocus={props.autofocus}
          onEnter={(text) => onEnter(props.prompt, text)}
          onUp={() => onUp()}
          onDown={() => onDown()}
          onEscape={(text) => onEscape(text)}
        />
      </div>
    );
  }
};
