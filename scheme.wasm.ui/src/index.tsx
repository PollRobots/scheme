import * as React from "react";
import * as ReactDOM from "react-dom";
import { Terminal } from "./components/Terminal";

interface AppState {
  theme: string;
}

const App: React.FunctionComponent<{}> = (props) => {
  const [state, setState] = React.useState<AppState>({ theme: "dark" });

  return (
    <div
      style={{
        display: "grid",
        height: "95vh",
        width: "90vw",
        margin: "0 auto",
        boxShadow: "#888 0 0.5em 1em",
      }}
    >
      <Terminal prompt="> " welcomeMessage="Welcome to scheme.wasm" />
    </div>
  );
};

ReactDOM.render(<App />, document.getElementById("app"));
