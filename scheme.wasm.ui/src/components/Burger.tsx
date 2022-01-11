import React from "react";
import { ThemeContext } from "./ThemeProvider";
import { reference } from "../util";
import animations from "../styles/animations.module.css";

reference(animations);

interface BurgerProps {
  open: boolean;
  visibleBack?: boolean;
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
  transformOrigin: "50% 50%",
  transition: "background 0.4s",
};

const kBurgerLineStyle: React.CSSProperties = {
  width: "1.5rem",
  height: "0.25rem",
  borderRadius: "0.125rem",
  position: "relative",
  transition: "background 0.4s",
};

export const Burger: React.FunctionComponent<BurgerProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  const ref = React.useRef<HTMLDivElement>(null);

  const burgerStyle = props.visibleBack
    ? {
        ...kBurgerStyle,
        background: props.open ? "transparent" : `${theme.foreground}80`,
        padding: "0.25em",
        borderRadius: "0.25em",
      }
    : kBurgerStyle;

  const lineStyle = {
    ...kBurgerLineStyle,
    background: props.open ? theme.foreground : theme.boldBackground,
  };
  return (
    <div
      ref={ref}
      onAnimationEnd={(e) => {
        if (ref.current) {
          ref.current.style.animation = "";
        }
      }}
      style={burgerStyle}
      onClick={() => {
        if (props.onClick) {
          props.onClick();
          if (ref.current) {
            ref.current.style.animation = "0.4s shake 0s";
          }
        }
      }}
    >
      <div style={lineStyle} />
      <div style={lineStyle} />
      <div style={lineStyle} />
    </div>
  );
};
