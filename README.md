# ob-yaml
Yaml to Python when executed.
It uses https://pypi.org/project/PyYAML/ and following code:

```python
import yaml
def main():
    return yaml.load("file path", yaml.SafeLoader)
```
