import React from "react";

interface EmergencyStopProps {
  msgs: string[];
  disabled?: boolean;
  title?: string;
  style?: React.CSSProperties;
  lastStyle?: React.CSSProperties;
  onAction: () => void;
}

export const EmergencyStop: React.FunctionComponent<EmergencyStopProps> = (
  props
) => {
  const [count, setCount] = React.useState(0);

  const click = () => {
    const newCount = count + 1;
    if (newCount >= props.msgs.length) {
      setCount(0);
      props.onAction();
    } else {
      setCount(newCount);
    }
  };

  const style =
    count == props.msgs.length - 1
      ? { ...props.style, ...props.lastStyle }
      : props.style;

  return (
    <button
      style={{ ...style, opacity: props.disabled ? 0.5 : undefined }}
      disabled={props.disabled}
      title={props.title}
      onBlur={() => setCount(0)}
      onClick={() => click()}
    >
      {props.msgs[count]}
    </button>
  );
};
