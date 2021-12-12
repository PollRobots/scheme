import React from "react";
import { TerminalData } from "./TerminalData";
import { TerminalInput } from "./TerminalInput";

interface TerminalProps {
  welcomeMessage?: React.ReactNode;
  prompt: string;
}

interface TerminalState {
  output: string[];
  history: string[];
  hidx: number;
  input: string;
  cached: string;
}

const kDefaultState: TerminalState = {
  output: [],
  history: [],
  hidx: 0,
  input: "",
  cached: "",
};

export const Terminal: React.FunctionComponent<TerminalProps> = (props) => {
  const [state, setState] = React.useState<TerminalState>({ ...kDefaultState });

  const onEnter = (text: string) => {
    const cmd = text;
    const history = [...state.history];

    // update history
    const hidx = history.findIndex((el) => el === cmd);
    if (hidx >= 0) {
      history.splice(hidx, 1);
    }
    history.push(cmd);
    setState({
      input: "",
      history: history,
      hidx: history.length,
      output: [...state.output, props.prompt + cmd],
      cached: "",
    });
  };

  const onUp = () => {
    console.log("up");
    if (state.hidx <= 0) {
      return;
    }
    setState({
      output: state.output,
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

  return (
    <div
      style={{
        backgroundColor: "#073642",
        color: "#eee8d5",
        fontFamily: "monospace",
        fontSize: "1rem",
        padding: "0.5em",
      }}
    >
      {props.welcomeMessage}
      <TerminalData text={state.output} />
      <TerminalInput
        prompt={props.prompt}
        value={state.input}
        onEnter={(text) => onEnter(text)}
        onUp={() => onUp()}
        onDown={() => onDown()}
      />
    </div>
  );
};
