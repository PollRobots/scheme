import React from "react";
import monaco from "monaco-editor";
import { registerLanguage } from "../monaco/scheme";
import { defineThemes } from "../monaco/solarized";

interface EditorProperties {
  height?: string | number;
  width?: string | number;
  maxHeight?: string | number;
  maxWidth?: string | number;
  theme?: string;
  defaultLanguage?: string;
  defaultValue?: string;
  onMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
}

const Editor: React.FunctionComponent<EditorProperties> = (props) => {
  const ref = React.useRef<HTMLDivElement>(null);
  const editorRef = React.useRef<monaco.editor.IStandaloneCodeEditor>();

  const resizeEditor = () => {
    if (editorRef.current && ref.current) {
      editorRef.current.layout({
        width: ref.current.offsetWidth,
        height: ref.current.offsetHeight,
      });
    }
  };

  React.useEffect(() => {
    let editor: monaco.editor.IStandaloneCodeEditor;
    if (ref.current) {
      registerLanguage();
      defineThemes();
      // @ts-ignore
      editor = window.monaco.editor.create(ref.current, {
        language: props.defaultLanguage,
        theme: props.theme,
        value: props.defaultValue,
      });
      editorRef.current = editor;
      window.addEventListener("resize", resizeEditor);
      if (props.onMount) {
        props.onMount(editor);
      }
    }
    return () => {
      window.removeEventListener("resize", resizeEditor);
      editorRef.current = undefined;
      editor.dispose();
    };
  }, []);

  return (
    <div
      style={{
        height: props.height,
        maxHeight: props.maxHeight,
        width: props.width,
        maxWidth: props.maxWidth,
      }}
      ref={ref}
    ></div>
  );
};

export default Editor;
