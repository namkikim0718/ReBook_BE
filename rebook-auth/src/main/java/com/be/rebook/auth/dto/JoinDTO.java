package com.be.rebook.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class JoinDTO {
    @NotEmpty(message = "username cannot be an empty string.")
    @Email
    private String username;

    @NotEmpty(message = "password cannot be an empty string.")
    private String password;
}
