import Editor, { Monaco } from "@monaco-editor/react";
import React from "react";
import { TerminalData } from "./TerminalData";
import { TerminalInput } from "./TerminalInput";
import { editor, KeyMod, KeyCode } from "monaco-editor";
import { kLanguageId, registerLanguage } from "../monaco/scheme";
import { defineThemes } from "../monaco/solarized";
import { EditorThemeContext, ThemeContext } from "./ThemeProvider";

interface TerminalProps {
  welcomeMessage?: React.ReactNode;
  prompt: string;
  pause: boolean;
  autofocus: boolean;
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
}

const kDefaultState: TerminalState = {
  output: [],
  history: [],
  hidx: 0,
  input: "",
  cached: "",
  editing: false,
  line: "",
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
  };

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
            background: editorTheme.foreground,
            padding: "0.25em",
            gridTemplateColumns: "1fr auto",
          }}
        >
          <span
            style={{
              color: editorTheme.background,
              margin: "auto 1em",
            }}
          >
            Edit current line
          </span>
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
          fontSize: "1rem",
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
