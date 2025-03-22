import argparse
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
from adjustText import adjust_text 

def bench_file(filepath, column="sent", name="", color="red", label=None):
    data = pd.read_csv(filepath, keep_default_na=False)

    data['service'] = data['mon_mon'] + data['proc_proc']
    data['sent'] -= data['inits']  # We don't want to count initiator calls

    data = data[['size', column]].groupby('size', as_index=False)[column].agg(['mean', 'std']).reset_index()
    # data = data[['size', column]].groupby('size', as_index=False).mean()

    plt.fill_between(data['size'], data['mean'] - data['std'], data['mean'] + data['std'], color=color, alpha=0.05)
    plt.plot(data['size'], data['mean'], label=label, color=color)
    plt.axhline(data['mean'].max(), color=color, linestyle="--", alpha=0.5, label=f"Max average = {data['mean'].max()} (at {data.loc[data['mean'].idxmax(), 'size']} processes)")

def bench(column, label, show=False):
    bench_file("benchmark_probe-delay_5000ms.csv", name="delayed (5s)", color="green", column=column, label="Probe delay = 5000ms")
    bench_file("benchmark_probe-delay_1000ms.csv", name="delayed (1s)", color="orange", column=column, label="Probe delay = 1000ms")
    bench_file("benchmark_no-probe-delay.csv", name="eager", color="red", column=column, label="Probe delay = 0")
    bench_file("benchmark_unmonitored.csv", name="unmonitored", color="blue", column=column, label="Unmonitored")

    # Labels and legend
    plt.xlabel("Number of services")
    plt.ylabel(f"Average {label}")
    plt.legend(loc='upper left', bbox_to_anchor=(1, 1))

    if show:
        plt.show()
    else:
        plt.savefig(f"benchmark_{column}.png", dpi=600, bbox_inches="tight")
        plt.close()

def plot_data_type(data, val):
    data['where'] = ((data['data_type'] == val) * (data['event_type'] == 'send')) + 0
    data = data[['timestamp', 'where']]
    plt.plot(data['timestamp'], data['where'].cumsum(), label=val)

def plot_states(data, state):
    data = data[data['data_type'] == state]
    for t in data['timestamp']:
        plt.axvline(x=t, color='red', linestyle='--')

def timeseries(filepath, label=None, show=False):
    data = pd.read_csv(filepath, keep_default_na=False)
    data['timestamp'] = pd.to_datetime(data['timestamp'], unit='us')

    plot_states(data, 'deadlocked')

    plot_data_type(data, 'query')
    plot_data_type(data, 'probe')
    plot_data_type(data, 'reply')

    # Labels and formatting
    plt.xlabel('Timestamp')
    plt.ylabel('Number of messages')
    plt.title(label)
    plt.xticks(rotation=45)
    plt.legend(loc='upper left', bbox_to_anchor=(1, 1))

    if show:
        plt.show()
    else:
        plt.savefig(Path(filepath).with_suffix(".png"), dpi=600, bbox_inches="tight")
        plt.close()

def gen_plots():
    bench('service', "number of service messages sent")
    bench('sent', "number of all messages sent")
    bench('probes', "number of probes sent")

    timeseries("timeseries_no-probe-delay_20k-proc.csv", label="No probe delay; 20k services")
    timeseries("timeseries_probe-delay_500ms_20k-proc.csv", label="Probe delay = 500ms; 20k services")


def main():
    parser = argparse.ArgumentParser(description="This is serious enterprise software.")

    parser.add_argument('-t', type=str, help="Timeseries CSV file", required=False)
    parser.add_argument('-b', type=str, help="Benchmark CSV file", required=False)

    # Parse arguments
    args = parser.parse_args()

    if args.t:
        timeseries(args.t, show=True)
    elif args.b:
        bench(args.b, show=True)
    else:
        gen_plots()


if __name__ == "__main__":
    main()
