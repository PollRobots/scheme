import React from "react";

interface TerminalDataProps {
  text: string[];
}

export const TerminalData: React.FunctionComponent<TerminalDataProps> = (
  props
) => {
  return (
    <div>
      {props.text.map((el, i) => {
        return (
          <div key={`line${i}`}>
            <span style={{ whiteSpace: "pre-wrap" }}>{el}</span>
          </div>
        );
      })}
    </div>
  );
};
