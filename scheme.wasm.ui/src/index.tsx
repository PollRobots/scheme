import * as React from "react";
import * as ReactDOM from "react-dom";
import { Terminal } from "./components/Terminal";
import { SchemeRuntime } from "./SchemeRuntime";

interface AppState {
  theme: string;
}

const App: React.FunctionComponent<{}> = (props) => {
  const [state, setState] = React.useState<AppState>({ theme: "dark" });
  const runtime = React.useRef<SchemeRuntime>();

  React.useEffect(() => {
    SchemeRuntime.load()
      .then((schemeRuntime) => {
        console.log("Loaded");
        runtime.current = schemeRuntime;
      })
      .catch((err) => console.error(err));
  }, ["once"]);

  const onInput = (str: string): string => {
    if (!runtime.current) {
      return "";
    }
    return runtime.current.processLine(str);
  };

  return (
    <div
      style={{
        display: "grid",
        height: "95vh",
        width: "90vw",
        margin: "0 auto",
        boxShadow: "#444 0 0.5em 1em",
        overflowY: "scroll",
      }}
    >
      <Terminal
        prompt="> "
        welcomeMessage="Welcome to scheme.wasm"
        onInput={(str) => onInput(str)}
      />
    </div>
  );
};

ReactDOM.render(<App />, document.getElementById("app"));
