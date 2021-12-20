import React from "react";
import { ThemeContext } from "./ThemeProvider";

interface IconButtonProps {
  size: number;
  title?: string;
  disabled?: boolean;
  onClick?: () => void;
}

interface IconButtonState {
  down: boolean;
  in: boolean;
}

export const IconButton: React.FunctionComponent<IconButtonProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  const [state, setState] = React.useState<IconButtonState>({
    down: false,
    in: false,
  });

  const isDown = !props.disabled && state.down;
  return (
    <div
      style={{
        cursor: props.disabled ? undefined : "pointer",
        background: isDown ? theme.boldBackground : theme.background,
        opacity: props.disabled ? 0.7 : 1,
        borderWidth: 1,
        borderStyle: "solid",
        borderColor: "#0008",
        display: "flex",
        width: props.size,
        height: props.size,
        boxSizing: "border-box",
        alignItems: "center",
        padding: isDown ? "6px 4px 2px 4px" : 4,
        boxShadow: isDown ? "#0004 0 2px 4px inset" : undefined,
      }}
      title={props.disabled ? undefined : props.title}
      onClick={props.onClick}
      onMouseDown={() => setState({ ...state, down: true })}
      onMouseUp={() => setState({ ...state, down: false })}
      onMouseEnter={() => setState({ ...state, in: true })}
      onMouseLeave={() => setState({ ...state, in: false, down: false })}
    >
      {props.children}
    </div>
  );
};
