#!/usr/bin/awk -f
# "paciente_id","paciente_datanascimento","estabelecimento_municipio_nome","vacina_categoria_codigo","vacina_dataaplicacao","vacina_descricao_dose","vacina_codigo"
# campos  2  4 19 23 28 29 30
BEGIN {
    FS=";"
}
{
    print $2";"$4";"$19";"$23";"$28";"$29";"$30
}
