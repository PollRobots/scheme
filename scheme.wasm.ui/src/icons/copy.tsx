import React from "react";
import { ThemeContext } from "../components/ThemeProvider";

interface IconProps {
  width?: string | number;
  height?: string | number;
}

export const Copy: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" fill="currentColor" transform="translate(-1,-4)">
        <path d="M 10,8 H 44 V 16 H 40 V 12 H 14 v 36 h 4 v 4 H 10 Z" />
        <g>
          <path d="m 26,24 h 26 v 28 h -8 v 8 H 26 Z m -4,-4 h 34 v 35 l -9,9 H 22 Z" />
          <g fill={theme.violet}>
            <path d="m 38,28 h 10 v 4 H 38 Z" />
            <path d="m 30,36 h 18 v 4 H 30 Z" />
            <path d="m 30,44 h 18 v 4 H 30 Z" />
            <path d="m 40,52 v 4 H 30 v -4 z" />
          </g>
        </g>
      </g>
    </svg>
  );
};

export const Paste: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g transform="translate(-64, -2)" fillRule="evenodd" fill="currentColor">
        <g>
          <path d="m 90,24 h 26 v 28 h -8 v 8 H 90 Z m -4,-4 h 34 v 35 l -9,9 H 86 Z" />
          <g fill={theme.violet}>
            <path d="m 102,28 h 10 v 4 h -10 z" />
            <path d="m 94,36 h 18 v 4 H 94 Z" />
            <path d="m 94,44 h 18 v 4 H 94 Z" />
            <path d="m 104,52 v 4 H 94 v -4 z" />
          </g>
        </g>
        <path
          d="m 97,8 a 2,2 0 0 1 -2,2 2,2 0 0 1 -2,-2 2,2 0 0 1 2,-2 2,2 0 0 1 2,2 z 
          M 95,4 C 90.581235,4.0004883 87,5.7903726 87,7.999512 h -9 
          c -2.20914,0 -4.000488,1.7913488 -4.000488,4.000488 V 56 
          C 73.999512,58.208651 75.79086,60 78,60 h 4 V 56 H 78 V 12 h 9 
          c 0,2.209139 1.791353,4.000488 4.000485,4.000488 H 99 
          c 2.20914,0 4.00048,-1.791349 4.00048,-4.000488 h 9 v 4.000488 
          h 4 V 12 c 0,-2.2091392 -1.78989,-4.000488 -3.99904,-4.000488 h -9 
          c 0,-2.2091394 -3.582692,-3.9990237 -8.000965,-3.9990237 z"
        />
      </g>
    </svg>
  );
};

export const Cut: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" transform="translate(-133.846, -0.327)">
        <path
          fill="currentColor"
          d="m 163.86257,36.914427 a 1.9827735,1.9827735 0 0 0 1.98278,1.982774 1.9827735,1.9827735 0 0 0 1.98277,-1.982774 1.9827735,1.9827735 0 0 0 -1.98277,-1.982773 1.9827735,1.9827735 0 0 0 -1.98278,1.982773 z m 13.54433,15.464404 a 6.7760923,4.6086652 48.44668 0 0 8.00915,2.845185 6.7760923,4.6086652 48.44668 0 0 -0.21711,-7.875394 6.7760923,4.6086652 48.44668 0 0 -8.00914,-2.845177 6.7760923,4.6086652 48.44668 0 0 0.2171,7.875386 z M 148,3.9999996 172.74826,41.121238 a 12.280209,8.3379957 49.407627 0 1 1.15366,-1.024689 12.280209,8.3379957 49.407627 0 1 14.41189,5.185032 12.280209,8.3379957 49.407627 0 1 0.39036,14.349157 12.280209,8.3379957 49.407627 0 1 -14.4119,-5.183866 12.280209,8.3379957 49.407627 0 1 -2.47343,-4.576257 l -0.0255,0.130117 c -0.79311,-3.172437 -3.96477,-7.138373 -5.55099,-7.931474 -1.58624,-0.793118 -5.55218,-1.585838 -5.55218,-1.585838 0,0 -11.89625,-19.034626 -13.48247,-21.413955 -1.58622,-2.379328 -2.3801,-6.344875 -1.58699,-9.5173125 C 146.41378,6.3797149 148,3.9999996 148,3.9999996 Z"
        />
        <path
          fill={theme.violet}
          d="m 183.69108,3.9996122 c 0,0 1.58623,2.3797155 2.37933,5.5521535 0.7931,3.1724373 -6e-4,7.1379843 -1.58699,9.5173153 -1.27002,1.905039 -7.94146,12.560605 -11.03228,17.501072 -1.91226,-2.868429 -3.82458,-5.736894 -5.73688,-8.605317 z m -29.1491,35.0869598 a 8.3379591,12.280169 40.591787 0 1 1.81703,0.341564 c 0.22795,0.364228 0.45635,0.727813 0.68429,1.092076 a 1.9201367,1.9201367 0 0 0 0.072,0.10572 c 0.25463,0.35286 0.64697,1.816486 2.35261,2.65351 a 1.9201367,1.9201367 0 0 0 0.0744,0.03367 c 1.0731,0.471155 1.88341,0.488197 2.22714,0.594832 a 1.9201367,1.9201367 0 0 0 0.0349,0.0116 c 0.47858,0.138518 0.85706,0.242257 1.25705,0.353181 -1.33966,1.620668 -2.68499,3.813409 -3.16353,5.727585 l -0.0256,-0.13012 a 8.3379591,12.280169 40.591787 0 1 -2.47343,4.576251 8.3379591,12.280169 40.591787 0 1 -14.4119,5.183872 8.3379591,12.280169 40.591787 0 1 0.39036,-14.349163 8.3379591,12.280169 40.591787 0 1 11.16472,-6.194617 z m -2.53965,4.877159 a 4.6086455,6.7760693 41.552729 0 0 -5.51033,3.385427 4.6086455,6.7760693 41.552729 0 0 -0.21725,7.874559 4.6086455,6.7760693 41.552729 0 0 8.00932,-2.845206 4.6086455,6.7760693 41.552729 0 0 0.21725,-7.874554 4.6086455,6.7760693 41.552729 0 0 -2.49899,-0.540226 z"
        />
      </g>
    </svg>
  );
};

export const Save: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" transform="translate(-202, -2)">
        <g fill="currentColor">
          <g>
            <path d="m 250,29 h -32 v -4 h 32 z" />
            <path d="m 250,18 h -32 v -4 h 32 z" />
          </g>
          <path
            d="m 254,8 h -40 v 28 h 40 z m 6,56 c 3,0 4,-1 4,-4 V 8 
             c 0,-3 -1,-4 -4,-4 h -52 c -3,0 -4,1 -4,4 v 48 c 0,2 0,2 1,3 l 4,4 
             c 1,1 1,1 3,1 h 6 V 40 h 34 v 24 z"
          />
        </g>
        <path
          fill={theme.violet}
          d="m 236,60 h -8 V 48 h 8 z m 12,-16 h -26 v 20 h 26 z"
        />
      </g>
    </svg>
  );
};

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

export const Undo: React.FunctionComponent<IconProps> = (props) => {
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" transform="translate(-344, 0)" fill="currentColor">
        <path
          id="path1073"
          d="M 371.99903,15.999512 356,32 371.99903,48.000488 V 36.976074 C 382.52612,36.888454 391.99999,36.893324 391.99999,44 l 3.99903,-0.06006 C 395.99885,28.826519 383.26757,27.19953 371.99903,27.023925 Z m 20.00096,32.000976 v 3.999024 h 3.99903 v -3.999024 z"
        />
      </g>
    </svg>
  );
};

export const Redo: React.FunctionComponent<IconProps> = (props) => {
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g transform="translate(64, 0) scale(-1, 1)">
        <g
          fillRule="evenodd"
          transform="translate(-344, 0)"
          fill="currentColor"
        >
          <path
            id="path1073"
            d="M 371.99903,15.999512 356,32 371.99903,48.000488 V 36.976074 C 382.52612,36.888454 391.99999,36.893324 391.99999,44 l 3.99903,-0.06006 C 395.99885,28.826519 383.26757,27.19953 371.99903,27.023925 Z m 20.00096,32.000976 v 3.999024 h 3.99903 v -3.999024 z"
          />
        </g>
      </g>
    </svg>
  );
};
