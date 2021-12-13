import React from "react";
import { ThemeContext } from "./ThemeProvider";

interface BurgerProps {
  open: boolean;
  onClick?: () => void;
}

const kBurgerStyle: React.CSSProperties = {
  position: "absolute",
  top: "1rem",
  left: "1rem",
  display: "flex",
  flexDirection: "column",
  justifyContent: "space-around",
  width: "1.5rem",
  height: "1.5rem",
  background: "transparent",
  border: "none",
  outline: "none",
  cursor: "pointer",
  padding: 0,
  zIndex: 10,
};

const kBurgerLineStyle: React.CSSProperties = {
  width: "1.5rem",
  height: "0.25rem",
  borderRadius: "0.125rem",
  transition: "all 0.3s linear",
  position: "relative",
  transformOrigin: "1px",
};

export const Burger: React.FunctionComponent<BurgerProps> = (props) => {
  const theme = React.useContext(ThemeContext);

  const lineStyle = {
    ...kBurgerLineStyle,
    background: props.open ? theme.background : theme.base01,
  };
  return (
    <div
      style={kBurgerStyle}
      onClick={() => {
        if (props.onClick) {
          props.onClick();
        }
      }}
    >
      <div
        style={{
          ...lineStyle,
          transform: props.open ? "rotate(45deg)" : "rotate(0)",
        }}
      />
      <div
        style={{
          ...lineStyle,
          opacity: props.open ? 0 : 1,
          transform: props.open ? "translateX(1em)" : "translateX(0)",
        }}
      />
      <div
        style={{
          ...lineStyle,
          transform: props.open ? "rotate(-45deg)" : "rotate(0)",
        }}
      />
    </div>
  );
};
