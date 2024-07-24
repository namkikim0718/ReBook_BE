package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.repository.MembersRepository;
import com.be.rebook.domain.members.dto.JoinDTO;
import com.be.rebook.global.config.BaseResponse;
import com.be.rebook.global.exception.ErrorCode;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class JoinService {

    private  final MembersRepository membersRepository;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;

    public JoinService(MembersRepository membersRepository, BCryptPasswordEncoder bCryptPasswordEncoder){
        this.membersRepository = membersRepository;
        this.bCryptPasswordEncoder = bCryptPasswordEncoder;
    }

    //아이디 생성 조건
    //25자 이하
    //특수문자 안됨
    private Boolean checkUsernameCharacters(String input){
        Boolean specialCharCheck = input.matches(".*[^a-zA-Z0-9].*");
        int inputLength = input.length();
        return inputLength <= 25 && !Boolean.TRUE.equals(specialCharCheck);
    }

    public BaseResponse<Members> joinProcess(JoinDTO joinDTO){
        String username = joinDTO.getUsername();
        HttpStatus returnStatus = null;
        String returnCode = null;
        String returnMessage = null;
        Members result = null;

        if(Boolean.FALSE.equals(checkUsernameCharacters(username))){
            //BAD_INPUT
            returnStatus = ErrorCode.BAD_INPUT.getStatus();
            returnCode = returnStatus.toString() + " failed";
            returnMessage = ErrorCode.BAD_INPUT.getMessage();
            return new BaseResponse<>(returnStatus,returnCode,returnMessage,result);
        }

        String password = joinDTO.getPassword();

        Boolean isExist = membersRepository.existsByUsername(username);

        if(Boolean.FALSE.equals(isExist)){
            //BAD_INPUT
            returnStatus = ErrorCode.EXISTING_USER_INFO.getStatus();
            returnCode = returnStatus.toString() + " failed";
            returnMessage = ErrorCode.EXISTING_USER_INFO.getMessage();
            return new BaseResponse<>(returnStatus,returnCode,returnMessage,result);
        }

        Members data = Members.builder()
                .username(username)
                .password(bCryptPasswordEncoder.encode(password+username))
                .role("ROLE_USER")
                .build();

        membersRepository.save(data);
        return new BaseResponse<>(data);
    }
}
