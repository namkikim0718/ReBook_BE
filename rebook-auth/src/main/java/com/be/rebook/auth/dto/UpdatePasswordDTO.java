package com.be.rebook.auth.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UpdatePasswordDTO {
    @NotNull(message = "Password cannot be null")
    @Size(min = 8, max = 100, message = "Password must be between 8 and 100 characters")
    @Pattern(regexp = "^[a-zA-Z0-9@#$%^&+=]*$", message = "Password can only contain alphanumeric characters and '@','#','$','%','^','&','+','='")
    private String password;
}

