PY ?=

test:
	cd tests && rm -f *.log
	cd tests && python${PY} -m unittest test_fortparser

ishell:
	python${PY} -c "import fasin;fasin.ishell()"

xform:
	cd tests && python${PY} -m unittest test_xform

cpp:
	cd tests && rm -f *.log
	cd tests && python${PY} -m unittest test_cpp

install:
	python${PY} setup.py install --user --record installed_files.txt

uninstall:
	cat installed_files.txt | xargs rm -rf
	rm -f installed_files.txt

init:
	pip install --user -r requirements.txt

clean:
	$(MAKE) -C docs clean
	rm -rf .cache build dist fasin.egg-info fasin.log
