import React from "react";
import { ThemeContext } from "../components/ThemeProvider";
import { IconProps } from "./IconProps";

export const Open: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" transform="translate(-272, 0)">
        <g transform="translate(0.09437056,-1.9997555)">
          <rect
            fill="currentColor"
            width="44"
            height="38.275318"
            x="297.66666"
            y="25.516878"
            rx="4"
            ry="4.2528133"
            transform="matrix(1,0,-0.33964446,0.9405539,0,0)"
          />
          <path
            fill={theme.violet}
            d="m 370.66602,10.666016 c -1.47734,0 -2.66602,1.869882 -2.66602,4.191406 v 6.285156 c 0,0.106181 0.005,0.210301 0.01,0.314453 -0.002,0.07138 -0.01,0.140838 -0.01,0.212891 v 45.974609 l 10.94531,-30.310547 c 1.83193,-5.073016 7.73492,-9.529296 13.23438,-9.529296 h 34.48633 V 21.669922 C 426.66602,18.528509 424.28865,16 421.33398,16 H 384 v -1.142578 c 0,-2.321524 -1.18868,-4.191406 -2.66602,-4.191406 z"
            transform="scale(0.75)"
          />
        </g>
      </g>
    </svg>
  );
};
