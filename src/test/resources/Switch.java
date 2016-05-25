class A {
    void main(List<String> args) {
        for (int a = start; a < line.length(); ++a) {
            if (line.charAt(a) == '(' && !inCode && !inQuote) {
                if (current == null)
                    current =
                            new InputException(line.substring(start, a).trim());
                start = a + 1;
                inCode = true;
            }
            if (line.charAt(a) == '"')
                inQuote = !inQuote;
            if (line.charAt(a) == ')' && !inQuote) {
                if (inCode) {
                    lines.offer(line.substring(start, a));
                    inCode = false;
                } else
                    end = true;
            }
            if (!end && a == line.length() - 1)
                line += r.readLine();
        }
        switch (state) {
            case "":
                String[] a = new String();
                String a = new String("");
                System.out.println();
                System.out.println();
                System.out.println();
                System.out.println();
                break;
            case 1:
                groupName = classInfo[2];
                packageName = classInfo[0].substring(2, classInfo[0].length() - 1);
                String a = new String("");
                break;
            case 3.2:
                String a = new String("");
            default:
                String a = new String("");
                break;
        }
    }
}