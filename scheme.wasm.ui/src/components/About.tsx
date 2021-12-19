import React from "react";
import { ThemeContext } from "./ThemeProvider";
import "../styles/animations.module.css";
import { reference } from "../util";
import animations from "../styles/animations.module.css";

reference(animations);

interface AboutState {
  loaded: boolean;
}

export const About: React.FunctionComponent = (props) => {
  const theme = React.useContext(ThemeContext);
  const [state, setState] = React.useState<AboutState>({ loaded: false });

  return (
    <div
      style={{
        display: state.loaded ? "flex" : "none",
        position: "absolute",
        width: "50em",
        height: "80vh",
        top: "10vh",
        left: "calc(50vw - 25em)",
        boxSizing: "border-box",
        background: theme.background,
        boxShadow: "rgba(0, 0, 0, 50%) 0em 0.5em 1em",
        transformOrigin: "50% 0%",
        animation: "0.3s zoom",
      }}
    >
      <iframe
        style={{ flex: 1, border: "none" }}
        src="./static/about.html"
        onLoad={() => setState({ loaded: true })}
      />
    </div>
  );
};
