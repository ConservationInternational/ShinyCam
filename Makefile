export GIT_SSL_NO_VERIFY := 1

app := team-ci
root_dir:= $(shell git rev-parse --show-toplevel)
venv_dir := $(root_dir)/env
pip := $(venv_dir)/bin/pip
requirements.txt := $(root_dir)/requirements.txt

venv:
	@test -d $(venv_dir) || (mkdir -p $(venv_dir) && virtualenv $(venv_dir))
	@touch $(venv_dir)/bin/activate

python: venv
	@echo ">>> Installing platform dependencies..."
	@$(pip) install -r $(requirements.txt)

notebook:
	source $(venv_dir)/bin/activate && jupyter notebook $(root_dir)

print-%  : ; @echo $* = $($*)

.PHONY: python notebook
